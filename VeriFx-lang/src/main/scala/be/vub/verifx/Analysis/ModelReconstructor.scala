package be.vub.verifx.Analysis

import be.vub.verifx.Compiler.Plugins.Z3CompilerPlugin.Z3Type
import com.microsoft.z3.{ArrayExpr, ArraySort, BoolSort, Context, DatatypeExpr, DatatypeSort, Expr, IntSort, Model, Sort, Z3Exception}

import scala.meta.{Term, Type}

/**
 * Represents a proof variable, i.e. a variable occuring in a top-level `forall` or `exists` statement of a proof
 */
case class Variable(name: String, value: Any, tpe: scala.meta.Type) {
  override def toString: String = s"$name: $tpe = $value"
}

/**
 * Represents the interpretation of a type parameter.
 * @param tpe The type parameter
 * @param values Its interpretation (i.e. the values that belong to this type, kind of like an enumeration).
 */
case class TypeParamDefinition(tpe: meta.Type, values: List[Term]) {
  override def toString: String = {
    val valuesStr = values.mkString(" | ")
    s"""enum $tpe {
       |  $valuesStr
       |}""".stripMargin
  }
}

/**
 * Defines methods to query/parse the counterexample returned by Z3 and turn it into a high-level counterexample.
 */
object ModelReconstructor {
  def reconstructUninterpretedSorts()(implicit model: Model, ctx: Context): List[TypeParamDefinition] = {
    model.getSorts().map(reconstructUSort).toList
  }

  private def reconstructUSort(sort: com.microsoft.z3.Sort)(implicit model: Model, ctx: Context): TypeParamDefinition = {
    val typeParamName = getUninterpretedSortName(sort.toString)
    val sortValues = model.getSortUniverse(sort).map(getUninterpretedSortValue(_, sort.toString())).sortBy(_.value).toList
    TypeParamDefinition(meta.Type.Name(typeParamName), sortValues)
  }

  def reconstructVars(varsAndType: Map[String, Z3Type])(implicit model: Model, ctx: Context): List[Variable] = {
    val consts = model.getConstDecls.filter(cnst => varsAndType.contains(cnst.getName.toString)) // only keep constants defined by the proof
    consts.map {
      case decl => {
        val name = decl.getName().toString
        val sort = decl.getRange().asInstanceOf[com.microsoft.z3.Sort]
        val z3Type = varsAndType.find(tup => name.endsWith(tup._1)).get._2
        val prefix = "eliminated_"
        val prefixStart = name.indexOf(prefix)
        reconstructVar(name, sort, z3Type).copy(name = name.substring(prefixStart + prefix.length))
      }
    }.toList
  }

  private def reconstructFields(fields: Array[com.microsoft.z3.Expr[_]])(implicit model: Model, ctx: Context): List[meta.Term] = {
    fields.map(reconstructExp).toList
  }

  private def getUninterpretedSortName(sortStr: String): String = {
    sortStr.substring(sortStr.indexOf("<") + 1, sortStr.lastIndexOf(">")) // original name
  }

  private def getUninterpretedSortValue(value: com.microsoft.z3.Expr[_], sortStr: String): meta.Term.Name = {
    val sort = getUninterpretedSortName(sortStr) // original name
    val name = value.getFuncDecl.getName().toString
      .replaceFirst(sortStr, sort)
      .replaceAllLiterally("!", "_") // Z3 will make instances of uninterpreted sorts and name them Sort!val!0, Sort!val!1, etc. We replace the ! by _
    meta.Term.Name(name)
  }

  // An array represents a set if the values are booleans
  private def isSetSort(sort: ArraySort[_, _]): Boolean = {
    sort.getRange.isInstanceOf[BoolSort]
  }

  // An array represents a map if the values are option types
  private def isMapSort(sort: ArraySort[_, _]): Boolean = {
    sort.getRange match {
      case s: DatatypeSort[_] => {
        s.getName.toString == "Option"
      }
      case _ => false
    }
  }

  private def reconstructExp(exp: com.microsoft.z3.Expr[_])(implicit model: Model, ctx: Context): meta.Term = {
    if (exp.isInt())
      meta.Lit.Int(exp.toString.toInt)
    else if (exp.isBool())
      meta.Lit.Boolean(exp.toString.toBoolean)
    else if (exp.isString()) {
      val str = exp.toString
      meta.Lit.String(str.substring(1, str.length()-1)) // remove the quotes around the string
    }
    else if (exp.isArray()) {
      val expSort = exp.getSort().asInstanceOf[ArraySort[_, _]]
      if (isSetSort(expSort))
        reconstructSet(exp.asInstanceOf[ArrayExpr[_, BoolSort]]).value()
      else if (isMapSort(expSort)) {
        reconstructMap(exp.asInstanceOf[ArrayExpr[_, _]]).value()
      }
      else {
        // the array represents a function
        val arrSort: ArraySort[_, _] = ctx.mkArraySort[IntSort](Array(ctx.mkIntSort(), ctx.mkIntSort()), ctx.mkIntSort())
        reconstructFunction(exp.asInstanceOf[ArrayExpr[_, _]]).value()
      }
    }
    else if (exp.getSort().isInstanceOf[com.microsoft.z3.UninterpretedSort]) {
      // the original name of the type parameter is between brackets < >
      val sortStr = exp.getSort().toString()
      getUninterpretedSortValue(exp, sortStr)
    }
    else {
      // it is a value from a generic type or a class/enum
      val functionName = exp.getFuncDecl.getName().toString.stripPrefix("mk-")
      val args = exp.getArgs()
      meta.Term.Apply(meta.Term.Name(functionName), reconstructFields(args))
    }
  }

  sealed trait SetModel {
    def add(element: scala.meta.Term): SetModel
    def remove(element: scala.meta.Term): SetModel
    def value(): Term

    def store(elemExpr: Expr[_], boolExpr: Expr[_])(implicit model: Model, ctx: Context): SetModel = {
      val reconstructedElem = reconstructExp(elemExpr)
      if (boolExpr.isTrue())
      // add it to the set
        add(reconstructedElem)
      else
      // remove it from the set
        remove(reconstructedElem)
    }
  }

  case class RegularSet(elements: Set[scala.meta.Term] = Set()) extends SetModel {
    def add(element: Term) = copy(elements = elements + element)
    def remove(element: Term) = copy(elements = elements - element)
    def value(): Term = meta.Term.Apply(Term.Name("Set"), elements.toList)
  }

  case class UniversalSet(notIncludedElements: Set[scala.meta.Term] = Set()) extends SetModel {
    def add(element: Term) = copy(notIncludedElements = notIncludedElements - element)
    def remove(element: Term) = copy(notIncludedElements = notIncludedElements + element)
    def value(): Term = {
      val universalSet = meta.Term.Name("⊤")
      val removedElementsSet = meta.Term.Apply(Term.Name("Set"), notIncludedElements.toList)
      meta.Term.ApplyInfix(universalSet, meta.Term.Name("--"), Nil, List(removedElementsSet))
    }
  }

  private def reconstructSet(exp: ArrayExpr[_, BoolSort])(implicit model: Model, ctx: Context): SetModel = {
    if (exp.isConstantArray) {
      // Definition of a constant set, can be  (as const (Array Key Bool) true) or (as const (Array Key Bool) false)
      // in the former case the set contains everything, in the latter case it contains nothing
      val boolValue = exp.getArgs()(0).toString.toBoolean
      if (boolValue)
      // set contains everything
        UniversalSet()
      else
      // empty set
        RegularSet()
    }
    else if (exp.isStore) {
      exp.getArgs match {
        case Array(setExpr, keyExpr, boolExpr) => {
          val set = reconstructSet(setExpr.asInstanceOf[ArrayExpr[_, BoolSort]])
          set.store(keyExpr, boolExpr)
        }
      }
    }
    else if (exp.isAsArray) {
      // the expression is of the form (_ as-array k!1) where k!1 is a function representing the set
      // we can evaluate this expression such that it yields the body of that function which should define the set
      val setExp = model.eval(exp.asInstanceOf[Expr[_]], true)
      reconstructSet(setExp.asInstanceOf[ArrayExpr[_, BoolSort]])
    }
    else
      throw new InternalError(s"Expected a constant array or a store expression but got something else: $exp")
  }

  sealed trait MapModel {
    def add(key: Term, value: Term): MapModel
    def remove(key: Term): MapModel
    def value(): Term

    def store(keyExpr: Expr[_], valueExpr: DatatypeExpr[_])(implicit model: Model, ctx: Context): MapModel = {
      val reconstructedKey = reconstructExp(keyExpr)
      if (isSome(valueExpr)) {
        // add this key-value binding
        val reconstructedValue = reconstructExp(valueExpr.getArgs()(0))
        add(reconstructedKey, reconstructedValue)
      }
      else {
        // remove this key
        remove(reconstructedKey)
      }
    }
  }

  case class RegularMap(dct: Map[Term, Term] = Map()) extends MapModel {
    def add(key: Term, value: Term): MapModel = copy(dct = dct + (key -> value))
    def remove(key: Term): MapModel = copy(dct = dct - key)
    def value(): Term = meta.Term.Apply(meta.Term.Name("Map"), dct.toList.map { case (key, value) => meta.Term.ApplyInfix(key, Term.Name("->"), Nil, List(value)) })
  }

  case class MapWithDefaultValue(defaultValue: Term, dct: Map[Term, Term] = Map(), removedKeys: Set[Term] = Set()) extends MapModel {
    def add(key: Term, value: Term): MapModel = copy(dct = dct + (key -> value))
    def remove(key: Term): MapModel = copy(dct = dct - key, removedKeys = removedKeys + key)
    def value(): Term = {
      // key -> value, ..., key -> /, _ -> defaultValue
      val presentKeyMapping = dct.toList.map { case (key, value) => meta.Term.ApplyInfix(key, Term.Name("->"), Nil, List(value)) }
      val absentKeyMapping = removedKeys.toList.map(Term.ApplyInfix(_, Term.Name("->"), Nil, List(Term.Name("/"))))
      val defaultMapping = List(Term.ApplyInfix(Term.Placeholder(), Term.Name("->"), Nil, List(defaultValue)))
      Term.Apply(Term.Name("Map"), presentKeyMapping ++ absentKeyMapping ++ defaultMapping)
    }
  }

  private def isSome(expr: DatatypeExpr[_]): Boolean = expr.getFuncDecl.getName.toString == "Some"

  private  def reconstructMap(exp: ArrayExpr[_, _])(implicit model: Model, ctx: Context): MapModel = {
    if (exp.isConstantArray) {
      // Definition of a constant map, can be ((as const (Array Key (Option Value))) (None)) or ((as const (Array Key (Option Value))) (Some value))
      // in the former case the map is empty, in the latter case every key maps to that value
      val dtExpr = exp.getArgs()(0).asInstanceOf[DatatypeExpr[_]]

      if (isSome(dtExpr)) {
        // A map that maps every key to the same value
        val value = dtExpr.getArgs()(0)
        val reconstructedValue = reconstructExp(value)
        MapWithDefaultValue(reconstructedValue)
      }
      else {
        // An empty map
        RegularMap()
      }
    }
    else if (exp.isStore) {
      exp.getArgs match {
        case Array(mapExpr, keyExpr, valueExpr) => {
          val map = reconstructMap(mapExpr.asInstanceOf[ArrayExpr[_, _]])
          map.store(keyExpr, valueExpr.asInstanceOf[DatatypeExpr[_]])
        }
      }
    }
    else if (exp.isAsArray) {
      // the expression is of the form (_ as-array k!1) where k!1 is a function representing the map
      // we can evaluate this expression such that it yields the body of that function which should define the map
      val mapExp = model.eval(exp, true)
      reconstructMap(mapExp.asInstanceOf[ArrayExpr[_, _]])
    }
    else
      throw new InternalError(s"Expected a constant array or a store expression but got something else: $exp")
  }

  case class Function(default: Term, domainSize: Int, mappings: Map[List[Term], Term] = Map()) {
    def store(argsAndRetExpr: List[Expr[_]])(implicit model: Model, ctx: Context): Function = {
      val argsAndRet = argsAndRetExpr.map(reconstructExp)
      argsAndRet match {
        case args :+ res => {
          copy(mappings = mappings + (args -> res))
        }
      }
    }

    private def mapping2Assignment(args: List[Term], res: Term) = {
      Term.ApplyInfix(Term.Tuple(args), Term.Name("->"), Nil, List(res))
      //Term.Assign(Term.Apply(Term.Name("f"), args), res)
    }

    def value(): Term.Block = {
      val assignments = mappings.map((mapping2Assignment _).tupled)
      val resAssignment = mapping2Assignment(List.fill(domainSize)(Term.Placeholder()), default)
      Term.Block(assignments.toList :+ resAssignment)
    }

    override def toString: String = {
      val funStr = value().stats.mkString("\n")
      s"Function:\n$funStr"
    }
  }

  private def determineDomainSize(sort: ArraySort[_, _]): Int = {
    // Determines the size of the domain (i.e. how many keys an array sort has)
    // The Java API does not expose such a method, however, we can compute it by asking the i-th domain sort until it throws an exception
    def hasDomain(i: Int): Boolean = {
      try {
        sort.getDomain(i)
        true
      } catch {
        case _ : Z3Exception => false
      }
    }

    LazyList
      .from(0)
      .takeWhile(hasDomain)
      .size
  }

  def reconstructFunction(exp: ArrayExpr[_, _])(implicit model: Model, ctx: Context): Function = {
    // Reconstruct the array as a function
    // simply display the function as a mapping from keys (i.e. arguments) to return value
    // e.g. f(1, 1) = 2
    //      f(2, 3) = 5
    //      f(_, _) = 9
    if (exp.isConstantArray) {
      val value = exp.getArgs()(0)
      val reconstructedValue = reconstructExp(value)
      val sort: ArraySort[_, _] = exp.getSort()
      val domainSize = determineDomainSize(sort)
      Function(reconstructedValue, domainSize)
    }
    else if (exp.isStore) {
      exp.getArgs.toList match {
        case funExpr :: argExprs => {
          val fun = reconstructFunction(funExpr.asInstanceOf[ArrayExpr[_, _]])
          fun.store(argExprs)
        }
      }
    }
    else
      throw new InternalError(s"Expected a constant array or a store expression but got something else: $exp")
  }

  private def reconstructType(tpe: Z3Type): meta.Type = {
    (tpe.typeArgs match {
      case Nil => meta.Type.Name(tpe.name)
      case args if tpe.name == "Array" => {
        // The array represents a function
        args match {
          case paramTypes :+ resType => {
            val params = paramTypes.map(reconstructType)
            val res = reconstructType(resType)
            Type.Function(params, res)
          }
        }
      }
      case args => meta.Type.Apply(meta.Type.Name(tpe.name), args.map(reconstructType))
    }).transform {
      case meta.Type.Name(name) if name.contains("<") => meta.Type.Name(getUninterpretedSortName(name))
    }.asInstanceOf[meta.Type]
  }

  private def reconstructVar(name: String, sort: com.microsoft.z3.Sort, tpe: Z3Type)(implicit model: Model, ctx: Context): Variable = {
    val value = model.eval(ctx.mkConst(name, sort), true)
    val reconstructedValue = reconstructExp(value)
    val reconstructedType = reconstructType(tpe)
    Variable(name, reconstructedValue, reconstructedType)
  }
}
