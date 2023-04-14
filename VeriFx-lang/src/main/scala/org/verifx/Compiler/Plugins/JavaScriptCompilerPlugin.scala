package org.verifx.Compiler.Plugins

import java.io.File

import org.verifx.Utilities.Interpolators._
import org.verifx.Compiler.IR.{Op, Term, Type}
import JavaScriptCompilerPlugin.{CompilerState, JSExp}
import cats.data.State
import cats.implicits._
import org.verifx.Compiler.{IR, Plugin}

object JavaScriptCompilerPlugin {
  final case class JSField(name: String, tpe: JSExp)

  /**
   * @param fields List of fields (name and their type) in the order they are expected by the constructor.
   * @param methods Maps method names to the actual method.
   * @param supert Optional reference to its super class.
   */
  final case class Clazz(fields: List[JSField], methods: Map[String, Method], supert: Option[Clazz]) {
    def getMethod(name: String): Option[Method] = {
      methods.get(name) match {
        case m @ Some(_) => m
        case None => supert.flatMap(_.getMethod(name))
      }
    }
  }

  /**
   * State that is kept during compilation
   * @param classesAndTraits Contains all classes and traits
   * @param fileClasses Classes defined in the current file
   */
  final class CompilerState(classesAndTraits: Map[String, Clazz] = Map(), fileClasses: Set[String] = Set()) {
    def get(className: String) = classesAndTraits.get(className)
    def add(name: String, clazz: Clazz) = new CompilerState(classesAndTraits + (name -> clazz), fileClasses + name)

    /**
     * @return All the classes defined in this file.
     */
    def getFileClasses() = fileClasses //classesAndTraits.view.filterKeys(fileClasses.contains).toMap
    def resetFile() = new CompilerState(classesAndTraits)
  }

  sealed trait JSExp {
    def addReturnStatement(): JSExp = this
  }

  case class Method(mods: List[JSExp], name: String, args: List[Plugin.Arg[JSExp]], body: JSExp, retTpe: JSExp) extends JSExp {
    def makeMethodDef(precondition: Option[Precondition]): String = {
      val argList = makeArgListFromArgs(args)
      val bodyWithReturn = body.addReturnStatement()

      precondition match {
        case None => {
          ind"""$name($argList) {
               |  $bodyWithReturn
               |}""".stripMargin
        }
        case Some(pre) => {
          val interpolationArgList = pre.args.map(_.name).map(name => "${" + s"$name" + "}").mkString(", ")
          val preArgList = makeArgListFromArgs(pre.args)
          val preName = s"__pre_$name"
          ind"""$preName($preArgList) {
               |  if (!${pre.body}) {
               |    throw Error(`Precondition failed for $name($interpolationArgList)`);
               |  }
               |}
               |
               |$name($argList) {
               |  this.$preName($argList);
               |  $bodyWithReturn
               |}""".stripMargin
        }
      }
    }
  }
  case class Precondition(method: String, args: List[Plugin.Arg[JSExp]], body: String) extends JSExp
  case class JSWrapper(code: String) extends JSExp {
    override def addReturnStatement(): JSWrapper = copy(s"return $code;")
    override def toString: String = code
  }

  case class If(condition: JSExp, thenb: JSExp, elseb: JSExp) extends JSExp {
    override def addReturnStatement(): If = copy(thenb = thenb.addReturnStatement, elseb = elseb.addReturnStatement)

    override def toString: String =
      ind"""if($condition) {
           |  $thenb
           |}
           |else {
           |  $elseb
           |}""".stripMargin
  }

  case class Block(block: List[JSExp]) extends JSExp {
    override def addReturnStatement(): JSExp = {
      val (stats, List(lastStat)) = block.splitAt(block.size - 1)
      copy(stats.appended(lastStat.addReturnStatement))
    }

    override def toString: String = block.mkString("\n")
  }

  implicit def String2JSWrapper(code: String) = JSWrapper(code)
  implicit def JSWrapper2String(code: JSExp) = code match {
    case JSWrapper(str) => str
    case _ => throw new InternalError(s"Expected ScalaWrapper but got something else: $code")
  }

  implicit def listJSExp2listString(lst: List[JSExp]): List[String] = lst.map(JSWrapper2String)

  private def makeArgList(args: List[JSExp]): String = args.mkString(", ")
  private def makeArgListFromArgs(args: List[Plugin.Arg[JSExp]]) = makeArgList(args.map(_.name))
}

class JavaScriptCompilerPlugin extends Plugin[JSExp, JSExp, CompilerState] {
  import JavaScriptCompilerPlugin._

  val int_type = ""
  val bool_type = ""
  val string_type = ""

  def makeComplexType(name: String, compiledTypeArgs: List[JSExp]): State[CompilerState, JSExp] = State.pure(name)

  def makeTypeParam(name: String, tparams: List[JSExp], upperTypeBound: Option[JSExp]): State[CompilerState, JSExp] = State.pure("")
  def makeFunctionType(paramTypes: List[JSExp], resType: JSExp): State[CompilerState, JSExp] = State.pure("")

  def makeLiteral(lit: Term.Literal): State[CompilerState, JSExp] = State.pure {
    lit match {
      case Term.BoolL(bool) => bool.toString
      case Term.IntL(value) => value.toString
      case Term.StringL(value) => s""""$value""""
    }
  }

  def makeBinaryOperation(op: Op.BinaryOperator, left: JSExp, right: JSExp): State[CompilerState, JSExp] = State.pure {
    val jsOperators: Map[Op.BinaryOperator, String] =
      Map(
        Op.Plus -> "+",
        Op.Minus -> "-",
        Op.Times -> "*",
        Op.Divide -> "/",
        Op.And -> "&&",
        Op.Or -> "||",
        Op.Equals -> "===",
        Op.NotEquals -> "!==",
        Op.SmallerThan -> "<",
        Op.SmallerOrEqual -> "<=",
        Op.BiggerThan -> ">",
        Op.BiggerOrEqual -> ">=")

    val jsOperator = jsOperators.get(op).get
    s"($left $jsOperator $right)"
  }

  def makeUnaryOperation(op: Op.UnaryOperator, arg: JSExp): State[CompilerState, JSExp] = State.pure {
    op match {
      case Op.Not => s"!$arg"
      case Op.Negative => s"-$arg"
    }
  }

  def makeReference(ref: String): State[CompilerState, JSExp] = State.pure(ref)

  def makeIf(condition: JSExp, thenb: JSExp, elseb: JSExp): State[CompilerState, JSExp] = State.pure(If(condition, thenb, elseb))

  def makeBlock(block: List[JSExp]): State[CompilerState, JSExp] = State.pure(Block(block))

  def instantiateClass(className: String, tparams: List[JSExp], args: List[JSExp]): State[CompilerState, JSExp] = State.pure {
    if (className == "Tuple") {
      val List(fst, snd) = args
      s"List([$fst, $snd])"
    }
    else if (className == "Vector") {
      // Initialize a list of the same size containing all nulls
      s"List()"
    }
    else {
      s"new $className(${makeArgList(args)})"
    }
  }

  def makeFieldAccess(classType: JSExp, obj: JSExp, field: String, fieldType: JSExp): State[CompilerState, JSExp] = State.pure(s"$obj.$field")
  def makeMethodReference(classType: JSExp, obj: JSExp, method: String, signature: JSExp): State[CompilerState, JSExp] = makeFieldAccess(classType, obj, method, "")

  def makeMethodCall(classType: JSExp, obj: JSExp, method: String, targs: List[JSExp], args: List[JSExp], argTypes: List[JSExp]): State[CompilerState, JSExp] = {
    import scala.meta._
    import scala.meta.{Type => MType}

    val argList = makeArgList(args)

    def isOfType(tpe: String, className: String): Boolean = {
      tpe.parse[MType].get match {
        case MType.Apply(MType.Name(`className`), _) => true
        case MType.Name(`className`) => true
        case _ => false
      }
    }

    val mapSpecialMethods = Map("add" -> "set", "remove" -> "delete", "contains" -> "has", "getOrElse" -> "get", "mapValues" -> "map")
    val mapTransformedMethods = Set("forall", "exists", "map", "filter", "combine", "zip", "keys", "values", "bijective")
    val setSpecialMethods = Map("remove" -> "delete", "forall" -> "every", "exists" -> "some", "diff" -> "subtract", "subsetOf" -> "isSubset")
    val vectorSpecialMethods = Map("write" -> "set", "append" -> "push", "forall" -> "every", "exists" -> "some")

    if (isOfType(classType, "Set") && method == "nonEmpty") {
      State.pure(s"!$obj.isEmpty()")
    }
    else if (isOfType(classType, "Map") && mapTransformedMethods.contains(method)) {
      val swapArgumentsMethods = Set("forall", "exists", "map", "filter")
      if (swapArgumentsMethods.contains(method)) {
        // This VeriFx method takes a function that expects a key and a value
        // but the immutable.js counterpart expects first the value and then the key
        // so wrap the function passed as argument in another function that swaps the order of the arguments
        val List(fn) = args
        for {
          funCall <- makeFunctionCall(fn, List("value", "key"))
          fun <- makeFunction(List(Plugin.Arg("key", ""), Plugin.Arg("value", "")), funCall)
        } yield s"$obj.$method($fun)"
      }
      else {
        method match {
          case "combine" => {
            val List(m2, f) = args
            State.pure(s"$obj.mergeWith($f, $m2)")
          }
          case "zip" => {
            val List(m2) = args
            val keys = s"$obj.keySeq().toSet().intersect($m2.keySeq().toSet())"
            val combined = s"$obj.mergeWith((oldv, newv) => [oldv, newv], $m2)"
            State.pure(s"$combined.filter((val, key) => $keys.contains(key))")
          }
          case "keys" =>
            State.pure(s"$obj.keySeq().toSet()")
          case "values" =>
            State.pure(s"$obj.valueSeq().toSet()")
          case "bijective" =>
            State.pure(s"$obj.valueSeq().toSet().size == $obj.size")
          case _ => throw new InternalError(s"Unhandled method on map: $method")
        }
      }
    }
    else if (isOfType(classType, "Tuple")) {
      // Tuples are represented as a list containing 2 elements
      method match {
        case "fst" => State.pure(s"$obj.get(0)")
        case "snd" => State.pure(s"$obj.get(1)")
        case _ => throw new InternalError(s"Unhandled method on tuple: $method")
      }
    }
    else {
      val jsMethod =
        if (isOfType(classType, "Map") && mapSpecialMethods.contains(method))
          mapSpecialMethods.get(method).get
        else if (isOfType(classType, "Set") && setSpecialMethods.contains(method))
          setSpecialMethods.get(method).get
        else if (isOfType(classType, "Vector") && vectorSpecialMethods.contains(method))
          vectorSpecialMethods.get(method).get
        else
          method

      State.pure(s"$obj.$jsMethod($argList)")
    }
  }

  def makeValDef(name: String, tpe: JSExp, value: JSExp): State[CompilerState, JSExp] = State.pure(s"const $name = $value;")
  def makeValDecl(name: String, tpe: JSExp): State[CompilerState, JSExp] = State.pure("") // can't declare constants in JS

  def makeArgListFromFields(fields: List[Plugin.Field[JSExp]]) = makeArgList(fields.map(_.name))

  def makeFunction(params: List[Plugin.Arg[JSExp]], body: JSExp): State[CompilerState, JSExp] = State.pure {
    val paramList = makeArgListFromArgs(params)
    val bodyWithReturn = body.addReturnStatement()
    s"""($paramList) => {
       |  $bodyWithReturn
       |}""".stripMargin
  }

  def makeFunctionCall(fun: JSExp, args: List[JSExp]): State[CompilerState, JSExp] = State.pure(s"$fun(${makeArgList(args)})")

  def makeMod(mod: IR.Mod.Mod): State[CompilerState, JSExp] = State.pure("") // ignore modifiers

  def makeMethodDef(name: String, mods: List[JSExp], tparams: List[JSExp], args: List[Plugin.Arg[JSExp]], body: JSExp, retTpe: JSExp, isRecursive: Boolean): State[CompilerState, JSExp] =
    State.pure(Method(mods, name, args, body, retTpe))

  def makeMethodDecl(mods: List[JSExp], name: String, tparams: List[JSExp], args: List[Plugin.Arg[JSExp]], retTpe: JSExp): State[CompilerState, JSExp] = State.pure("") // can't declare methods in JS

  // Ignore the predicates, they are only useful for Z3
  def makeContextDef(name: String, tparams: List[JSExp], args: List[Plugin.Arg[JSExp]], body: JSExp): State[CompilerState, JSExp] = State.pure("")
  def makePreconditionDef(name: String, tparams: List[JSExp], args: List[Plugin.Arg[JSExp]], body: JSExp): State[CompilerState, JSExp] = State.pure(Precondition(name, args, body))
  def makeInvariantDef(name: String, tparams: List[JSExp], args: List[Plugin.Arg[JSExp]], body: JSExp): State[CompilerState, JSExp] = State.pure("")

  def makeClassDef(annots: List[String], name: String, tparams: List[JSExp], fields: List[Plugin.Field[JSExp]], methodsAndPreds: List[JSExp], ms: List[JSExp], supert: Option[JSExp]): State[CompilerState, JSExp] =
    State[CompilerState, JSExp] {
      classTable => {
        // Create an internal class representation
        val methods = methodsAndPreds.filter(_.isInstanceOf[Method]).asInstanceOf[List[Method]].map(m => (m.name, m)).toMap
        val superTrait = supert.map(superType => classTable.get(superType) match {
          case Some(s) => s
          case None => throw new Error(s"type $superType not found in class table:\n$classTable")
        })

        val jsFields = fields.map { case Plugin.Field(_, name, tpe) => JSField(name, tpe) }
        val thisClazz = Clazz(jsFields, methods, superTrait)

        // Compile the trait
        val argList = makeArgListFromFields(fields)
        val initState = fields.map(_.name).map(field => s"this.$field = $field;").mkString("\n")
        val constructor =
          s"""constructor($argList) {
             |  $initState
             |}""".stripMargin
        val constructorAndMethods =
          if (methods.isEmpty) constructor
          else (constructor :: makeClassBody(methodsAndPreds, thisClazz).filter(_ != "")).mkString("\n\n")

        val extendsSuper = supert.map(tpe => s" extends $tpe").getOrElse("")
        val classDef =
          ind"""class $name$extendsSuper {
               |  $constructorAndMethods
               |}""".stripMargin


        // Add this class to the class table
        (classTable.add(name, thisClazz), classDef)
      }
    }

  def makeTraitDef(mods: List[JSExp], name: String, tparams: List[JSExp], stats: List[JSExp], supert: Option[JSExp]): State[CompilerState, JSExp] =
    State[CompilerState, JSExp] {
      classTable => {
        // Create an internal class representation
        val methods = stats.filter(_.isInstanceOf[Method]).asInstanceOf[List[Method]].map(m => (m.name, m)).toMap
        val superTrait = supert.map(superType => classTable.get(superType).get)
        val thisClazz = Clazz(Nil, methods, superTrait)

        // Compile the trait
        // JS does not have traits by default
        // Implement a trait as a class whose constructor takes no parameters
        val constructor = "constructor() {}"
        val constructorAndStats =
          if (stats.isEmpty) constructor
          else (constructor :: makeClassBody(stats, thisClazz).filter(_ != "")).mkString("\n\n")

        val extendsSuper = supert.map(tpe => s" extends $tpe").getOrElse("")
        val traitDef =
          ind"""class $name$extendsSuper {
               |  $constructorAndStats
               |}""".stripMargin

        // Add this trait to the class table
        (classTable.add(name, thisClazz), traitDef)
      }
    }

  def makeEnumDef(name: String, tparams: List[JSExp], ctors: List[Plugin.EnumCtor[JSExp]]): State[CompilerState, JSExp] = {
    import cats.implicits._

    for {
      enumSupertypeDef <- makeTraitDef(List(), name, tparams, List(), None)
      ctorDefs <- ctors traverse {
        case Plugin.EnumCtor(ctorName, tparams, fields) => {
          makeClassDef(List(), ctorName, tparams, fields, List(), List(), Some(name))
        }
      }
    } yield (enumSupertypeDef :: ctorDefs).mkString("\n")
  }

  private def getClazz(classTpe: JSExp): State[CompilerState, Clazz] = State.inspect[CompilerState, Clazz] {
    case state => {
      val className = classTpe.toString
      state.get(className) match {
        case Some(clazz) => clazz
        case None => throw new InternalError(s"Class $className not found.")
      }
    }
  }

  private def getCaseType(casee: Plugin.Case[JSExp]): Option[JSExp] = casee match {
    case Plugin.Case(Plugin.Pat.Typed(_, tpe), _) => Some(tpe.asInstanceOf[JSExp])
    case Plugin.Case(Plugin.Pat.Extract(tpe, _), _) => Some(tpe.asInstanceOf[JSExp])
    case Plugin.Case(Plugin.Pat.Wildcard, _) => None
  }

  private def isWildcardPattern(casee: Plugin.Case[JSExp]): Boolean = casee match {
    case Plugin.Case(Plugin.Pat.Wildcard, _) => true
    case _ => false
  }

  // Optionally returns a list of variables and the values with which they match.
  // e.g. `case Insert(p, c) => ...` will return Some(List(("p", expBeingMatched.p), ("c", expBeingMatched.c)))
  //      `case _ => ...` will return None
  private def getIdentifiersAndBody(casee: Plugin.Case[JSExp], expBeingMatched: JSExp): State[CompilerState, (Option[List[(String, JSExp)]], JSExp)] = casee match {
    case Plugin.Case(Plugin.Pat.Typed(Plugin.Pat.Var(name), _), body) => State.pure {
      (Some(List((name, expBeingMatched))), body)
    }
    case Plugin.Case(Plugin.Pat.Typed(Plugin.Pat.Wildcard, _), body) => State.pure {
      (None, body)
    }
    case Plugin.Case(Plugin.Pat.Wildcard, body) => State.pure {
      (None, body)
    }
    case casee @ Plugin.Case(Plugin.Pat.Extract(_, vars), body) => {
      val ctorTpe = getCaseType(casee).get

      for {
        clazz <- getClazz(ctorTpe)
        fieldAccesses <- clazz.fields.traverse { case JSField(name, tpe) => makeFieldAccess(ctorTpe, expBeingMatched, name, tpe) }
        varsBoundToFields <- State.pure[CompilerState, List[(String, JSExp)]] {
          (vars zip fieldAccesses)
            .filter(_._1 != Plugin.Pat.Wildcard)
            .asInstanceOf[List[(Plugin.Pat.Var, JSExp)]]
            .map(_ match { case (Plugin.Pat.Var(name), fieldAccess) => (name, fieldAccess) })
        }
      } yield (Some(varsBoundToFields), body)
    }
  }

  private def makeCaseBody(exp: JSExp, casee: Plugin.Case[JSExp]): State[CompilerState, JSExp] = getIdentifiersAndBody(casee, exp).flatMap {
    case (None, body) => State.pure(body)
    case (Some(identifiersAndValues), body) => {
      val identifiersStr = identifiersAndValues.map { case (identifier, exp) => s"let $identifier = $exp;" }.mkString("\n")
      makeBlock(
        List(
          identifiersStr,
          body))
    }
  }

  // JavaScript does not support pattern matching
  // so we translate the pattern match into nested if statement
  def makePatternMatch(exp: JSExp, cases: List[Plugin.Case[JSExp]]): State[CompilerState, JSExp] = cases match {
    case casee :: Nil => makeCaseBody(exp, casee)
    case casee :: _ if isWildcardPattern(casee) => throw new InternalError(s"Wildcard pattern is not the last case.")
    case casee :: rest => {
      val tpe = getCaseType(casee).get // safe to fetch the type of the data constructor that matches this case because it is not a wildcard pattern and hence must match a data constructor
      // Check if `exp` matches type `tpe`
      val classCheck = s"$exp instanceof $tpe"
      for {
        caseBody <- makeCaseBody(exp, casee)
        transformedRest <- makePatternMatch(exp, rest)
      } yield If(classCheck, caseBody, transformedRest)
    }
  }

  private def makeClassBody(stats: List[JSExp], clazz: Clazz) = {
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
        val argsStr = method.args.map(arg => arg.name).mkString(", ")
        val preName = s"__pre_$methodName"
        val newBody =
          ind"""this.$preName($argsStr);
               |super.$methodName($argsStr)""".stripMargin

        val methodDef = method.copy(body = newBody).makeMethodDef(None)
        val preArgList = args.map(arg => arg.name).mkString(", ")
        val interpolationArgList = args.map(_.name).map(name => "${" + s"$name" + "}").mkString(", ")
        ind"""$preName($preArgList) {
             |  if (!$body) {
             |    throw Error(`Precondition failed for $methodName($interpolationArgList)`);
             |  }
             |}
             |
             |$methodDef""".stripMargin
      }
      case _: Precondition => "" // already handled by the corresponding method
      case JSWrapper(str) => str
    }
  }

  def makeImport(pkg: String): State[CompilerState, JSExp] = State.pure {
    val file = pkg.replace('.', File.separatorChar)
    s"require('./$file')"
  }

  // ignore logical variables because we don't compile quantified formulas to JS
  def makeLogicVar(name: String, tpe: JSExp): State[CompilerState, JSExp] = State.pure("")

  def startFile(path: Option[String]): State[CompilerState, JSExp] = State[CompilerState, JSExp](s => (s.resetFile(), ""))
  def endFile(): State[CompilerState, JSExp] = State[CompilerState, JSExp](s => {
    val exports = s.getFileClasses().map(clazz => s"module.exports.$clazz = $clazz;").mkString("\n")
    (s.resetFile(), exports)
  })

  def run(compiler: State[CompilerState, List[JSExp]], initialState: Option[CompilerState]): (CompilerState, String) = {
    val initState = initialState.getOrElse(new CompilerState())
    compiler.run(initState).value match {
      case (tbl, compiledClasses) => {
        val program =
          ind"""const { Map, Set } = require('immutable');
               |
               |${compiledClasses.mkString("\n\n")}""".stripMargin
        (tbl, program)
      }
    }
  }
}
