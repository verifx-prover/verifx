package org.verifx.Compiler.Plugins

import org.verifx.Utilities.Interpolators._
import org.verifx.Compiler.IR.{Op, Term, Type}
import ScalaCompilerPlugin.{ClassTable, ScalaExp}
import cats.data.State
import cats.implicits._
import org.verifx.Compiler.{IR, Plugin, ProjectCompiler}

object ScalaCompilerPlugin {
  final case class Clazz(methods: Map[String, Method], supert: Option[Clazz]) {
    def getMethod(name: String): Option[Method] = {
      methods.get(name) match {
        case m @ Some(_) => m
        case None => supert.flatMap(_.getMethod(name))
      }
    }
  }

  /**
   * @param classesAndTraits Maps class names to the corresponding class.
   * @param enumParams Maps data constructors of enumerations to the list of type parameters.
   *                   Unused type parameters are replaced by the `Nothing` type.
   */
  final case class ClassTable(classesAndTraits: Map[String, Clazz] = Map(), enumParams: Map[String, List[String]] = Map())

  sealed trait ScalaExp
  case class Method(mods: List[ScalaExp], name: String, tparams: List[ScalaExp], args: List[Plugin.Arg[ScalaExp]], body: ScalaExp, retTpe: ScalaExp) extends ScalaExp {
    def makeMethodDef(precondition: Option[Precondition], withReturnType: Boolean = true) = {
      val nameAndTParams = if (tparams.isEmpty) name else s"$name[${tparams.mkString(", ")}]"
      val argList = makeTypedArgList(args)
      val modsStr = makeModsString(mods)
      val retStr = if (withReturnType) s": $retTpe" else ""
      precondition match {
        case None =>
          ind"""${modsStr}def $nameAndTParams($argList)$retStr = {
               |  $body
               |}""".stripMargin
        case Some(pre) => {
          val preArgList = makeTypedArgList(pre.args)
          val argsStr = args.map(_.name).mkString(", ")
          val preName = s"__pre_$nameAndTParams"
          ind"""private def $preName($preArgList) = {
               |  assert(${pre.body})
               |}
               |
               |${modsStr}def $nameAndTParams($argList)$retStr = {
               |  $preName($argsStr)
               |  $body
               |}""".stripMargin
        }
      }
    }
  }
  case class Precondition(method: String, args: List[Plugin.Arg[ScalaExp]], body: String) extends ScalaExp
  case class ScalaType(name: String, tparams: List[ScalaType]) extends ScalaExp {
    override def toString: String = s"$name[${tparams.mkString(", ")}]"
  }
  case class ScalaWrapper(code: String) extends ScalaExp {
    override def toString: String = code
  }
  case class MethodRef(ref: String) extends ScalaExp {
    override def toString: String = s"ref _"
  }

  implicit def String2ScalaWrapper(code: String) = ScalaWrapper(code)
  implicit def ScalaWrapper2String(code: ScalaExp) = code match {
    case ScalaWrapper(str) => str
    case MethodRef(ref) => ref
    case _ => throw new InternalError(s"Expected ScalaWrapper but got something else: $code")
  }
  //implicit def listA2listB[A, B](lst: List[A])(implicit transformer: A => B): List[B] = lst.map(transformer)
  implicit def listScalaExp2listString(lst: List[ScalaExp]): List[String] = lst.map(ScalaWrapper2String)

  private def makeArgList(args: List[String]): String = args.mkString(", ")

  private def makeModsString(mods: List[ScalaExp]): ScalaExp = {
    val modss = mods.mkString(" ")
    if (modss == "")
      modss
    else
      s"$modss "
  }

  private def makeTypedArgList(args: List[Plugin.Arg[ScalaExp]]): ScalaExp = {
    val typedArgs = args.map(arg => s"${arg.name}: ${arg.tpe}")
    makeArgList(typedArgs)
  }

  private def makeTypedFieldList(fields: List[Plugin.Field[ScalaExp]]): ScalaExp = {
    val typedFields = fields.map(field => {
      if (field.mods.isEmpty)
        s"${field.name}: ${field.tpe}"
      else {
        val mods = makeModsString(field.mods)
        s"$mods${field.name}: ${field.tpe}"
      }
    })
    makeArgList(typedFields)
  }

  val veriFxOps = "org.verifx.Runtime.Implicits"
}

class ScalaCompilerPlugin extends Plugin[ScalaExp, ScalaExp, ClassTable] {
  import ScalaCompilerPlugin._

  val int_type = "Int"
  val bool_type = "Boolean"
  val string_type = "String"

  def makeComplexType(name: String, compiledTypeArgs: List[ScalaExp]): State[ClassTable, ScalaExp] = State.inspect {
    case s => {
      // so when creating a type, check if it is a data ctor
      // if so, then zip its type params with the one from super (so stored in `enums`)
      // then filter out the ones that are Nothing
      // then keep only the original tparams (i.e. tuple._1)

      if (compiledTypeArgs.isEmpty)
        name
      else if (name == "Tuple")
        s"(${makeArgList(compiledTypeArgs)})"
      else {
        val newCompiledTypeArgs = s.enumParams.get(name) match {
          case None => compiledTypeArgs
          case Some(tparams) => (compiledTypeArgs zip tparams).filter(_._2 != "Nothing").map(_._1)
        }

        if (newCompiledTypeArgs.isEmpty)
          name
        else
          s"$name[${makeArgList(newCompiledTypeArgs)}]"
      }
    }
  }

  def makeFunctionType(paramTypes: List[ScalaExp], resType: ScalaExp): State[ClassTable, ScalaExp] = State.pure {
    val argTypes = paramTypes.mkString(", ")
    s"($argTypes) => $resType"
  }

  def makeTypeParam(name: String, tparams: List[ScalaExp], upperTypeBound: Option[ScalaExp]): State[ClassTable, ScalaExp] = {
    val state = tparams match {
      case Nil => State.pure[ClassTable, ScalaExp](name)
      case tparams => {
        for {
          tpe <- makeComplexType(name, tparams)
        } yield tpe
      }
    }

    upperTypeBound match {
      case None =>
        state
      case Some(upperType) =>
        state.map(scalaTypeParam => s"$scalaTypeParam <: $upperType")
    }
  }

  def makeLiteral(lit: Term.Literal): State[ClassTable, ScalaExp] = State.pure {
    lit match {
      case Term.BoolL(bool) => bool.toString
      case Term.IntL(value) => value.toString
      case Term.StringL(value) => s""""$value""""
    }
  }

  def makeBinaryOperation(op: Op.BinaryOperator, left: ScalaExp, right: ScalaExp): State[ClassTable, ScalaExp] = State.pure {
    val scalaOperators: Map[Op.BinaryOperator, String] =
      Map(
        Op.Plus -> "+",
        Op.Minus -> "-",
        Op.Times -> "*",
        Op.Divide -> "/",
        Op.And -> "&&",
        Op.Or -> "||",
        Op.Equals -> "==",
        Op.NotEquals -> "!=",
        Op.SmallerThan -> "<",
        Op.SmallerOrEqual -> "<=",
        Op.BiggerThan -> ">",
        Op.BiggerOrEqual -> ">=")

    val scalaOperator = scalaOperators.get(op).get
    s"($left $scalaOperator $right)"
  }

  def makeUnaryOperation(op: Op.UnaryOperator, arg: ScalaExp): State[ClassTable, ScalaExp] = State.pure {
    op match {
      case Op.Not => s"!($arg)"
      case Op.Negative => s"-($arg)"
    }
  }

  def makeReference(ref: String): State[ClassTable, ScalaExp] = State.pure(ref)

  def makeIf(condition: ScalaExp, thenb: ScalaExp, elseb: ScalaExp): State[ClassTable, ScalaExp] = State.pure {
    ind"""if($condition) {
         |  $thenb
         |}
         |else {
         |  $elseb
         |}""".stripMargin
  }

  def makeBlock(block: List[ScalaExp]): State[ClassTable, ScalaExp] = State.pure(block.mkString("\n"))

  def instantiateClass(className: String, tparams: List[ScalaExp], args: List[ScalaExp]): State[ClassTable, ScalaExp] = {
    val clazz = makeComplexType(className, tparams)
    if (className == "Tuple")
      State.pure[ClassTable, ScalaExp](s"(${makeArgList(args)})")
    else if (className == "LList")
      instantiateClass("List", tparams, args)
    else if (className == "Vector" && args.nonEmpty)
      instantiateClass(className, tparams, List()) // since our built-in Vector data type has default arguments it is added when instantiating an empty Vector. Remove them when compiling to Scala.
    else
      clazz.map(clz => s"$clz(${makeArgList(args)})")
  }

  def makeFieldAccess(classType: ScalaExp, obj: ScalaExp, field: String, fieldType: ScalaExp): State[ClassTable, ScalaExp] = State.pure {
    import scala.meta._
    import scala.meta.{Type => MType}

    def isTuple(tpe: String): Boolean = {
      tpe.parse[MType].get match {
        case MType.Tuple(_) => true
        case _ => false
      }
    }

    val tupleFields = Map("fst" -> "_1", "snd" -> "_2")
    val newField = if (isTuple(classType)) tupleFields.get(field).get else field

    s"$obj.$newField"
  }

  def makeMethodReference(classType: ScalaExp, obj: ScalaExp, method: String, signature: ScalaExp): State[ClassTable, ScalaExp] = makeFieldAccess(classType, obj, method, "").map(MethodRef(_))

  def makeMethodCall(classType: ScalaExp, obj: ScalaExp, method: String, targs: List[ScalaExp], args: List[ScalaExp], argTypes: List[ScalaExp]): State[ClassTable, ScalaExp] = State.pure {
    import scala.meta._
    import scala.meta.{Type => MType}

    val argList = makeArgList(args)

    def isOfType(tpe: String, className: String): Boolean = {
      tpe.parse[MType].get match {
        case MType.Apply(MType.Name(`className`), _) => true
        case MType.Tuple(_) => className == "Tuple"
        case MType.Name(`className`) => true
        case _ => false
      }
    }

    val mapSpecialMethods = Map("get" -> "gett", "mapValues" -> "mappValues", "zip" -> "zipp") // methods that need to be renamed because they exist on Scala's map but are different than in VeriFx

    if (isOfType(classType, "Map") && mapSpecialMethods.contains(method)) {
      val newMethodName = mapSpecialMethods.get(method).get
      s"$obj.$newMethodName($argList)"
    }
    else if (isOfType(classType, "Set") && (method == "isEmpty" || method == "nonEmpty"))
      s"$obj.$method" // isEmpty and nonEmpty are fields in Scala, not methods
    else if (method == "asInstanceOf")
      s"$obj.asInstanceOf[${makeArgList(targs)}]"
    else
      s"$obj.$method($argList)"
  }

  def makeValDef(name: String, tpe: ScalaExp, value: ScalaExp): State[ClassTable, ScalaExp] = State.pure(s"val $name: $tpe = $value")
  def makeValDecl(name: String, tpe: ScalaExp): State[ClassTable, ScalaExp] = State.pure(s"val $name: $tpe")

  def makeFunction(params: List[Plugin.Arg[ScalaExp]], body: ScalaExp): State[ClassTable, ScalaExp] = State.pure {
    val paramList = makeTypedArgList(params)
    ind"""($paramList) => {
         |  $body
         |}""".stripMargin
  }

  def makeFunctionCall(fun: ScalaExp, args: List[ScalaExp]): State[ClassTable, ScalaExp] = State.pure(s"$fun(${makeArgList(args)})")

  def makeMod(mod: IR.Mod.Mod): State[ClassTable, ScalaExp] = State.pure(
    mod match {
      case IR.Mod.Private => "private"
      case IR.Mod.Protected => "protected"
      case IR.Mod.Annot(name) => s"@$name"
      case IR.Mod.Override => "override"
      case IR.Mod.Sealed => "sealed"
    })

  def makeMethodDef(name: String, mods: List[ScalaExp], tparams: List[ScalaExp], args: List[Plugin.Arg[ScalaExp]], body: ScalaExp, retTpe: ScalaExp, isRecursive: Boolean): State[ClassTable, ScalaExp] =
    State.pure(Method(mods, name, tparams, args, body, retTpe))

  def makeMethodDecl(mods: List[ScalaExp], name: String, tparams: List[ScalaExp], args: List[Plugin.Arg[ScalaExp]], retTpe: ScalaExp): State[ClassTable, ScalaExp] = State.pure {
    val argList = makeTypedArgList(args)
    val modsStr = makeModsString(mods)
    ind"""${modsStr}def $name($argList): $retTpe"""
  }

  // Ignore the context and invariant, they are only useful in Z3
  def makeContextDef(name: String, tparams: List[ScalaExp], args: List[Plugin.Arg[ScalaExp]], body: ScalaExp): State[ClassTable, ScalaExp] = State.pure("")
  def makePreconditionDef(name: String, tparams: List[ScalaExp], args: List[Plugin.Arg[ScalaExp]], body: ScalaExp): State[ClassTable, ScalaExp] = State.pure(Precondition(name, args, body))
  def makeInvariantDef(name: String, tparams: List[ScalaExp], args: List[Plugin.Arg[ScalaExp]], body: ScalaExp): State[ClassTable, ScalaExp] = State.pure("")

  def makeClassDef(annots: List[String], name: String, tparams: List[ScalaExp], fields: List[Plugin.Field[ScalaExp]], methodsAndPreds: List[ScalaExp], ms: List[ScalaExp], supert: Option[ScalaExp]): State[ClassTable, ScalaExp] = {
    val clazz = makeComplexType(name, tparams)
    val typedArgs = makeTypedFieldList(fields)

    for {
      clz <- clazz
      classDef <- State[ClassTable, ScalaExp] {
        classTable => {
          // Create an internal class representation
          val methods = methodsAndPreds.filter(_.isInstanceOf[Method]).asInstanceOf[List[Method]].map(m => (m.name, m)).toMap
          val superTrait = supert.map(superType => classTable.classesAndTraits.get(ScalaWrapper2String(superType).split('[').head).get)
          val thisClazz = Clazz(methods, superTrait)

          // Compile the class
          val body =
            if (methodsAndPreds.isEmpty)
              ""
            else {
              ind"""{
                   |  ${makeClassBody(methodsAndPreds, thisClazz).filter(_ != "").mkString("\n\n")}
                   |}""".stripMargin
            }

          val annotations = annots.map(a => s"@$a").mkString("\n")
          val extendsSuper = supert.map(tpe => s" extends $tpe").getOrElse("")
          val clazz = ind"""case class $clz($typedArgs)$extendsSuper $body"""

          val defn =
            if (annotations.isEmpty)
              clazz
            else
              ind"""$annotations
                   |$clazz""".stripMargin

          // Add this class to the class table
          (classTable.copy(classesAndTraits = classTable.classesAndTraits + (name -> thisClazz)), defn)
        }
      }
    } yield classDef
  }

  def makeTraitDef(mods: List[ScalaExp], name: String, tparams: List[ScalaExp], stats: List[ScalaExp], supert: Option[ScalaExp]): State[ClassTable, ScalaExp] = {
    val traitt = makeComplexType(name, tparams)

    for {
      traitType <- traitt
      traitDef <- State[ClassTable, ScalaExp] {
        classTable => {
          // Create an internal class representation
          val methods = stats.filter(_.isInstanceOf[Method]).asInstanceOf[List[Method]].map(m => (m.name, m)).toMap
          val superTrait = supert.map(superType => classTable.classesAndTraits.get(ScalaWrapper2String(superType).split('[').head).get)
          val thisClazz = Clazz(methods, superTrait)

          // Compile the trait
          val body =
            if (stats.isEmpty) ""
            else
              ind"""{
                   |  ${makeClassBody(stats, thisClazz).filter(_ != "").mkString("\n\n")}
                   |}""".stripMargin

          val modsStr = makeModsString(mods)
          val extendsSuper = supert.map(tpe => s" extends $tpe").getOrElse("")
          val traitDef = ind"""trait $traitType$extendsSuper $body"""

          val defn = ind"""$modsStr$traitDef""".stripMargin

          // Add this trait to the class table
          (classTable.copy(classesAndTraits = classTable.classesAndTraits + (name -> thisClazz)), defn)
        }
      }
    } yield traitDef
  }

  private def makeClassBody(stats: List[ScalaExp], clazz: Clazz) = {
    val methods = stats.filter(_.isInstanceOf[Method]).asInstanceOf[List[Method]]
    val methodNames = methods.map(_.name).toSet

    val pres = stats.filter(_.isInstanceOf[Precondition]).asInstanceOf[List[Precondition]].map(pre => (pre.method, pre)).toMap
    val preNames = pres.keySet
    val presWithoutMethod = preNames -- methodNames // preconditions that do not have a method in this class (i.e. the method must be defined in a super trait)

    stats.map {
      case m: Method => m.makeMethodDef(pres.get(m.name))
      case Precondition(methodName, args, body) if presWithoutMethod.contains(methodName) => {
        // Precondition associated to a method that is defined in a super trait
        // Override the method here and in the body assert the precondition and then call the super method
        val method = clazz.getMethod(methodName).get
        val argNames = method.args.map(arg => arg.name).mkString(", ")
        val preName = s"__pre_${method.name}"
        val newBody =
          ind"""$preName($argNames)
               |super.$methodName($argNames)""".stripMargin

        val argsWithNewTypes = (method.args zip args) map {
          case (Plugin.Arg(methodArgName, _), Plugin.Arg(_, preArgType)) => Plugin.Arg(methodArgName, preArgType)
        }
        val methodStr = method.copy(args = argsWithNewTypes, body = newBody).makeMethodDef(None, false)
        val argList = makeTypedArgList(args)
        ind"""private def $preName($argList) = {
             |  assert($body)
             |}
             |
             |override $methodStr""".stripMargin
      }
      case _: Precondition => "" // already handled by the corresponding method
      case ScalaWrapper(str) => str
    }
  }

  def makeEnumDef(name: String, tparams: List[ScalaExp], ctors: List[Plugin.EnumCtor[ScalaExp]]): State[ClassTable, ScalaExp] = {
    for {
      ctrait <- makeTraitDef(List("sealed"), name, tparams.map(tparam => s"+$tparam"), List(), None)
      ctorDefsAndTParams <- ctors traverse {
        case Plugin.EnumCtor(ctorName, _, fields) => {
          val enumType = s"$name[${tparams.mkString(", ")}]"
          makeClassDef(List(), ctorName, tparams, fields, List(), List(), Some(enumType)) map {
            case compiledClass => (compiledClass, tparams)
          }
        }
      }
      _ <- State.modify[ClassTable] {
        case s => {
          // Add this constructor to the class table and remember which type parameters it uses
          val ctorsAndTParams = ctors zip ctorDefsAndTParams.map(_._2)
          val extendedEnums = s.enumParams ++ ctorsAndTParams.map {
            case (ctor, tparams) =>
              (ctor.name, tparams.map(_.toString))
          }

          s.copy(enumParams = extendedEnums)
        }
      }
    } yield (ctrait :: ctorDefsAndTParams.map(_._1)).mkString("\n")
  }

  private def makePattern(pat: Plugin.Pat.Pat): String = pat match {
    case Plugin.Pat.Typed(Plugin.Pat.Var(name), tpe) => s"$name: $tpe"
    case Plugin.Pat.Typed(Plugin.Pat.Wildcard, tpe) => s"_: $tpe"
    case Plugin.Pat.Wildcard => "_"
    case Plugin.Pat.Extract(ctorTpe, vars) => {
      val varsStr = vars.map {
        case Plugin.Pat.Var(name) => name
        case Plugin.Pat.Wildcard => "_"
      }.mkString(", ")

      val ctor = ctorTpe.toString.split('[')(0)
      s"$ctor($varsStr)"
    }
  }

  private def makeCase(casee: Plugin.Case[ScalaExp]): String = casee match {
    case Plugin.Case(pat, body) => {
      val patStr = makePattern(pat)
      ind"""case $patStr => {
           |  $body
           |}""".stripMargin
    }
  }

  def makePatternMatch(exp: ScalaExp, cases: List[Plugin.Case[ScalaExp]]): State[ClassTable, ScalaExp] = State.pure {
    val casesStr = cases.map(makeCase).mkString("\n")
    ind"""$exp match {
         |  $casesStr
         |}""".stripMargin
  }

  def makeImport(pkg: String): State[ClassTable, ScalaExp] = State.pure(s"import $pkg")

  // ignore logical variables because we don't compile quantified formulas to Scala
  def makeLogicVar(name: String, tpe: ScalaExp): State[ClassTable, ScalaExp] = State.pure("")

  def startFile(maybePath: Option[String]): State[ClassTable, ScalaExp] = State.pure {
    import org.verifx.Utilities.Utils.RichString

    maybePath match {
      case Some(path) => {
        // VeriFx imports files.
        // Remove the filename to get the package
        val pkgStr = ProjectCompiler.filePathToPkg(path)
        val pkg = pkgStr.stripSuffixAfter(".")

        s"""package $pkg
           |import $veriFxOps._""".stripMargin
      }
      case None => s"import $veriFxOps._"
    }
  }

  def endFile(): State[ClassTable, ScalaExp] = State.pure("")

  def run(compiler: State[ClassTable, List[ScalaExp]], initialState: Option[ClassTable]): (ClassTable, String) = {
    val state = initialState.getOrElse(ClassTable())
    compiler.run(state).value match {
      case (tbl, compiledClasses) =>
        (tbl, compiledClasses.mkString("\n\n"))
    }
  }
}
