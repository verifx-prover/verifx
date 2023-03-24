package be.vub.kdeporre.verifx.Compiler.Plugins

import be.vub.kdeporre.verifx.Compiler.IR.{Op, Term, Type}
import be.vub.kdeporre.verifx.Compiler.{IR, Plugin, ProgramCompiler}
import cats.data.State
import cats.implicits._

import scala.collection.SortedMap

object Z3CompilerPlugin {
  sealed trait Z3Exp {
    def fillTypeParameters(substitutions: Map[Z3Type, Z3Type]): State[CompilerState, Z3Exp]
  }

  final case class ValDef(name: String, tpe: Z3Exp, value: Z3Exp) extends Z3Exp {
    def fillTypeParameters(substitutions: Map[Z3Type, Z3Type]): State[CompilerState, Z3Exp] =
      for {
        filledTpe <- tpe.fillTypeParameters(substitutions)
        filledValue <- value.fillTypeParameters(substitutions)
      } yield copy(tpe = filledTpe, value = filledValue)
  }


  final case class Z3Wrapper(exp: String) extends Z3Exp {
    override def toString: String = exp
    def fillTypeParameters(substitutions: Map[Z3Type, Z3Type]): State[CompilerState, Z3Exp] = State.pure(this)
  }

  sealed trait Mod extends Z3Exp {
    def fillTypeParameters(substitutions: Map[Z3Type, Z3Type]): State[CompilerState, Z3Exp] = State.pure(this)
  }
  case object Private extends Mod
  case object Protected extends Mod
  case object Sealed extends Mod

  sealed trait MethodOrPredicate extends Z3Exp {
    val name: String
    val args: List[Plugin.Arg[Z3Exp]]
    val body: Z3Exp
  }

  implicit class FillableArg(arg: Plugin.Arg[Z3Exp]) {
    def fillTypeParameters(substitutions: Map[Z3Type, Z3Type]): State[CompilerState, Plugin.Arg[Z3Exp]] = {
      arg.tpe.fillTypeParameters(substitutions).map(filledTpe => arg.copy(tpe = filledTpe))
    }
  }

  implicit class FillableField(field: Plugin.Field[Z3Exp]) {
    def fillTypeParameters(substitutions: Map[Z3Type, Z3Type]): State[CompilerState, Plugin.Field[Z3Exp]] = for {
      filledMods <- field.mods.traverse(_.fillTypeParameters(substitutions))
      filledTpe <- field.tpe.fillTypeParameters(substitutions)
    } yield field.copy(mods = filledMods, tpe = filledTpe)
  }

  final case class MethodWithFullName(methodName: MethodName, method: Method) extends Z3Exp {
    def fillTypeParameters(substitutions: Map[Z3Type, Z3Type]): State[CompilerState, Z3Exp] = for {
      filledMethodName <- methodName.fillTypeParameters(substitutions)
      filledMethod <- method.fillTypeParameters(substitutions)
    } yield copy(filledMethodName.asInstanceOf[MethodName], filledMethod.asInstanceOf[Method])
  }

  /**
   * @param name The method's name
   */
  final case class Method(mods: List[Z3Exp], name: String, tparams: List[Z3Type], args: List[Plugin.Arg[Z3Exp]], body: Z3Exp, retTpe: Z3Type, isRecursive: Boolean,
                          context: Option[Ctx], precondition: Option[Pre], invariant: Option[Inv]) extends MethodOrPredicate {
    def fillTypeParameters(substitutions: Map[Z3Type, Z3Type]): State[CompilerState, Z3Exp] = for {
      filledTParams <- tparams.traverse(tparam => tparam.fillTypeParameters(substitutions))
      filledArgs <- args.traverse(arg => arg.fillTypeParameters(substitutions))
      filledBody <- body.fillTypeParameters(substitutions)
      filledRetTpe <- retTpe.fillTypeParameters(substitutions)
      filledContext <- context.traverse(_.fillTypeParameters(substitutions))
      filledPrecondition <- precondition.traverse(_.fillTypeParameters(substitutions))
      filledInvariant <- invariant.traverse(_.fillTypeParameters(substitutions))
    } yield copy(tparams = filledTParams.asInstanceOf[List[Z3Type]], args = filledArgs,
                 body = filledBody, retTpe = filledRetTpe.asInstanceOf[Z3Type],
                 context = filledContext.asInstanceOf[Option[Ctx]],
                 precondition = filledPrecondition.asInstanceOf[Option[Pre]],
                 invariant = filledInvariant.asInstanceOf[Option[Inv]])

    def setFullName(className: String, classTParams: List[Z3Type]) = MethodWithFullName(MethodName(className, classTParams, name, tparams), this)
  }

  final case class MethodWithoutSpec(mods: List[Z3Exp], name: String, tparams: List[Z3Type], args: List[Plugin.Arg[Z3Exp]], body: Z3Exp, retTpe: Z3Type, isRecursive: Boolean) extends MethodOrPredicate {
    def extend(ctx: Option[Ctx], pre: Option[Pre], inv: Option[Inv]) =
      Method(mods, name, tparams, args, body, retTpe, isRecursive, ctx, pre, inv)

    def fillTypeParameters(substitutions: Map[Z3Type, Z3Type]): State[CompilerState, Z3Exp] = for {
      filledTParams <- tparams.traverse(tparam => tparam.fillTypeParameters(substitutions))
      filledArgs <- args.traverse(arg => arg.fillTypeParameters(substitutions))
      filledBody <- body.fillTypeParameters(substitutions)
      filledRetTpe <- retTpe.fillTypeParameters(substitutions)
    } yield copy(tparams = filledTParams.asInstanceOf[List[Z3Type]], args = filledArgs, body = filledBody, retTpe = filledRetTpe.asInstanceOf[Z3Type])
  }

  sealed trait Predicate extends MethodOrPredicate
  final case class Ctx(name: String, args: List[Plugin.Arg[Z3Exp]], body: Z3Exp) extends Predicate {
    def fillTypeParameters(substitutions: Map[Z3Type, Z3Type]): State[CompilerState, Z3Exp] = for {
      filledArgs <- args.traverse(arg => arg.fillTypeParameters(substitutions))
      filledBody <- body.fillTypeParameters(substitutions)
    } yield copy(args = filledArgs, body = filledBody)
  }

  final case class Pre(name: String, args: List[Plugin.Arg[Z3Exp]], body: Z3Exp) extends Predicate {
    def fillTypeParameters(substitutions: Map[Z3Type, Z3Type]): State[CompilerState, Z3Exp] = for {
      filledArgs <- args.traverse(arg => arg.fillTypeParameters(substitutions))
      filledBody <- body.fillTypeParameters(substitutions)
    } yield copy(args = filledArgs, body = filledBody)
  }

  final case class Inv(name: String, args: List[Plugin.Arg[Z3Exp]], body: Z3Exp) extends Predicate {
    def fillTypeParameters(substitutions: Map[Z3Type, Z3Type]): State[CompilerState, Z3Exp] = for {
      filledArgs <- args.traverse(arg => arg.fillTypeParameters(substitutions))
      filledBody <- body.fillTypeParameters(substitutions)
    } yield copy(args = filledArgs, body = filledBody)
  }

  implicit def string2Z3Wrapper(str: String) = Z3Wrapper(str)

  /**
   * Represents a type in Z3.
   * Enumerations are treated specially because their data constructors are not types of their own in Z3.
   * Hence, data constructor type are compiled to the type of the enumeration.
   * However, to construct an instance of the enumeration we need the data constructor but the type got compiled to the enumeration's type.
   * Therefore, `Z3Type` also keeps an `ogDataCtorTpe` which stores the "type" of the data constructor that got replaced by the enumeration's type.
   * If this type is not an enumeration type, then `ogDataCtorTpe` is `None`.
   */
  final case class Z3Type(name: String, typeArgs: List[Z3Type], ogDataCtorTpe: Option[Z3Type] = None) extends Z3Exp {
    override def toString: Z3Code =
      if (typeArgs.isEmpty)
        name
      else
        s"($name ${typeArgs.mkString(" ")})"

    def fillTypeParameters(substitutions: Map[Z3Type, Z3Type]): State[CompilerState, Z3Exp] = {
      substitutions.get(this) match {
        case Some(newTpe) => State.pure[CompilerState, Z3Exp](newTpe)
        case None => {
          for {
            filledTypeArgs <- typeArgs.traverse(_.fillTypeParameters(substitutions))
          } yield copy(typeArgs = filledTypeArgs.asInstanceOf[List[Z3Type]])
        }
      }
    }

    def getDataCtorTpe(): Z3Type = {
      val ogTpe = ogDataCtorTpe.getOrElse(this)
      ogTpe.copy(typeArgs = ogTpe.typeArgs.map(_.getDataCtorTpe()))
    }
  }

  final case class BinaryOperation(operator: String, left: Z3Exp, right: Z3Exp) extends Z3Exp {
    def fillTypeParameters(substitutions: Map[Z3Type, Z3Type]): State[CompilerState, Z3Exp] = for {
      filledLeft <- left.fillTypeParameters(substitutions)
      filledRight <- right.fillTypeParameters(substitutions)
    } yield copy(left = filledLeft, right = filledRight)


    override def toString: Z3Code = s"($operator $left $right)"
  }

  final case class UnaryOperation(operator: String, arg: Z3Exp) extends Z3Exp {
    def fillTypeParameters(substitutions: Map[Z3Type, Z3Type]): State[CompilerState, Z3Exp] = for {
      filledArg <- arg.fillTypeParameters(substitutions)
    } yield copy(arg = filledArg)

    override def toString: Z3Code = s"($operator $arg)"
  }

  final case class If(condition: Z3Exp, thenb: Z3Exp, elseb: Z3Exp) extends Z3Exp {
    def fillTypeParameters(substitutions: Map[Z3Type, Z3Type]): State[CompilerState, Z3Exp] = for {
      filledCondition <- condition.fillTypeParameters(substitutions)
      filledThen <- thenb.fillTypeParameters(substitutions)
      filledElse <- elseb.fillTypeParameters(substitutions)
    } yield copy(condition = filledCondition, thenb = filledThen, elseb = filledElse)

    override def toString: Z3Code =
      s"""(ite $condition
         |  $thenb
         |  $elseb)""".stripMargin
  }

  final case class QualifiedIdentifier(identifier: Z3Exp, tpe: Z3Exp) extends Z3Exp {
    def fillTypeParameters(substitutions: Map[Z3Type, Z3Type]): State[CompilerState, Z3Exp] = for {
      filledIdentifier <- identifier.fillTypeParameters(substitutions)
      filledTpe <- tpe.fillTypeParameters(substitutions)
    } yield copy(identifier = filledIdentifier, tpe = filledTpe)

    override def toString: Z3Code = s"(as $identifier $tpe)"
  }

  final case class Apply(op: Z3Exp, args: List[Z3Exp]) extends Z3Exp {
    def fillTypeParameters(substitutions: Map[Z3Type, Z3Type]): State[CompilerState, Z3Exp] = for {
      filledOp <- op.fillTypeParameters(substitutions)
      filledArgs <- args.traverse(_.fillTypeParameters(substitutions))
    } yield copy(op = filledOp, args = filledArgs)

    override def toString: Z3Code = {
      if (args.isEmpty)
        op.toString
      else
        s"($op ${args.mkString(" ")})"
    }
  }

  final case class MethodCall(classType: Z3Type, op: MethodName, targs: List[Z3Type], args: List[Z3Exp]) extends Z3Exp {
    def fillTypeParameters(substitutions: Map[Z3Type, Z3Type]): State[CompilerState, Z3Exp] = for {
      filledClass <- classType.fillTypeParameters(substitutions).asInstanceOf[State[CompilerState, Z3Type]]
      filledTArgs <- targs.traverse(targ => targ.fillTypeParameters(substitutions).asInstanceOf[State[CompilerState, Z3Type]])
      _ <- concretizeClassIfNeeded(op.method, filledClass, filledTArgs) // important to concretize the class type such that the method is created if it does not yet exist
      filledOp <- op.asInstanceOf[Z3Exp].fillTypeParameters(substitutions)
      filledArgs <- args.traverse(_.fillTypeParameters(substitutions))
    } yield copy(classType = filledClass, op = filledOp.asInstanceOf[MethodName], targs = filledTArgs, args = filledArgs)

    override def toString: Z3Code = {
      if (args.isEmpty)
        op.toString
      else
        s"($op ${args.mkString(" ")})"
    }
  }

  final case class Quantifier(quantifier: String, vars: List[Z3Exp], body: Z3Exp) extends Z3Exp {
    def fillTypeParameters(substitutions: Map[Z3Type, Z3Type]): State[CompilerState, Z3Exp] = for {
      filledVars <- vars.traverse(_.fillTypeParameters(substitutions))
      filledBody <- body.fillTypeParameters(substitutions)
    } yield copy(vars = filledVars, body = filledBody)

    override def toString: Z3Code = {
      val varsStr = vars.mkString(" ")

      s"""($quantifier ($varsStr)
         |  $body)""".stripMargin
    }
  }

  def Exists(vars: List[Z3Exp], body: Z3Exp) = Quantifier("exists", vars, body)
  def Forall(vars: List[Z3Exp], body: Z3Exp) = Quantifier("forall", vars, body)

  final case class Var(name: String, tpe: Z3Exp) extends Z3Exp {
    def fillTypeParameters(substitutions: Map[Z3Type, Z3Type]): State[CompilerState, Z3Exp] = for {
      filledTpe <- tpe.fillTypeParameters(substitutions)
    } yield copy(tpe = filledTpe)

    override def toString: Z3Code = s"($name $tpe)"
  }

  case class MethodName(className: String, classTypeParams: List[Z3Type], method: String, methodTypeParams: List[Z3Type]) extends Z3Exp {
    def fillTypeParameters(substitutions: Map[Z3Type, Z3Type]): State[CompilerState, Z3Exp] = {
      val clazz = Z3Type(className, classTypeParams)
      for {
        filledClazz <- clazz.fillTypeParameters(substitutions).asInstanceOf[State[CompilerState, Z3Type]]
        filledMethodTypeParams <- methodTypeParams.traverse(tparam => tparam.fillTypeParameters(substitutions).asInstanceOf[State[CompilerState, Z3Type]])
      } yield copy(className = filledClazz.asInstanceOf[Z3Type].name, classTypeParams = filledClazz.typeArgs, methodTypeParams = filledMethodTypeParams)
    }

    override def toString: Z3Code = makeFunName(className, classTypeParams, method, methodTypeParams)
  }

  sealed trait Case extends Z3Exp

  /**
   * Case that matches ADT constructor.
   * e.g.: `case Insert(pos, char, _) => ...`
   * @param tpe Name of the constructor
   * @param vars Variables used in the pattern (in order)
   */
  case class CtorCase(tpe: Z3Exp, vars: List[String]) extends Case {
    def fillTypeParameters(substitutions: Map[Z3Type, Z3Type]): State[CompilerState, Z3Exp] = for {
      compiledTpe <- tpe.fillTypeParameters(substitutions)
    } yield copy(tpe = compiledTpe)

    override def toString: Z3Code = {
      if (vars.isEmpty)
        s"mk-$tpe"
      else
        s"(mk-$tpe ${vars.mkString(" ")})"
    }
  }

  /**
   * Case that matches any expression (e.g. `case _ => ...` or `case x => ...`)
   */
  case class VariableCase(variable: String) extends Case {
    def fillTypeParameters(substitutions: Map[Z3Type, Z3Type]): State[CompilerState, Z3Exp] = State.pure(this)
    override def toString: Z3Code = variable
  }

  case class Pattern(casee: Case, body: Z3Exp) extends Z3Exp {
    def fillTypeParameters(substitutions: Map[Z3Type, Z3Type]): State[CompilerState, Z3Exp] = {
      for {
        compiledCase <- casee.fillTypeParameters(substitutions)
        compiledBody <- body.fillTypeParameters(substitutions)
      } yield Pattern(compiledCase.asInstanceOf[Case], compiledBody)
    }

    override def toString: Z3Code = s"($casee $body)"
  }

  case class Match(exp: Z3Exp, cases: List[Z3Exp]) extends Z3Exp {
    def fillTypeParameters(substitutions: Map[Z3Type, Z3Type]): State[CompilerState, Z3Exp] = {
      for {
        filledExp <- exp.fillTypeParameters(substitutions)
        filledCases <- cases.traverse(_.fillTypeParameters(substitutions))
      } yield Match(filledExp, filledCases)
    }

    override def toString: Z3Code =
      s"""(match $exp (
         |  ${cases.mkString("\n")}))""".stripMargin
  }

  object Lambda {
    def apply(vars: List[Z3Exp], body: Z3Exp) = Quantifier("lambda", vars, body)
  }

  object Let {
    def apply(vars: List[Z3Exp], body: Z3Exp) = Quantifier("let", vars, body)
  }

  object Equals {
    def apply(x: Z3Exp, y: Z3Exp) = Apply("=", List(x, y))
  }

  object Select {
    def apply(exps: Z3Exp*) = Apply("select", exps.toList)
  }

  object Implies {
    def apply(antecedent: Z3Exp, consequent: Z3Exp) = Apply("=>", List(antecedent, consequent))
  }

  object Or {
    def apply(x: Z3Exp, y: Z3Exp) = BinaryOperation("or", x, y)
  }

  object And {
    def apply(x: Z3Exp, y: Z3Exp) = BinaryOperation("and", x, y)
  }

  object Not {
    def apply(exp: Z3Exp) = UnaryOperation("not", exp)
  }

  object Somee {
    def apply(value: Z3Exp, valueType: Z3Type) = {
      val maybeValueType = Z3Type("Option", List(valueType))
      val qualifiedSomeValue = QualifiedIdentifier("Some", maybeValueType)
      Apply(qualifiedSomeValue, List(value))
    }
  }

  object Nonee {
    def apply(valueType: Z3Type) = {
      val maybeValueType = Z3Type("Option", List(valueType))
      QualifiedIdentifier("None", maybeValueType)
    }
  }

  object Get {
    def apply(exp: Z3Exp, tpe: Z3Exp) = Apply(QualifiedIdentifier("get", tpe), List(exp))
  }

  val intT: Z3Type = Z3Type("Int", Nil)
  val boolT: Z3Type = Z3Type("Bool", Nil)
  val stringT: Z3Type = Z3Type("String", Nil)
  val primitives = Set[Z3Exp](intT, boolT, stringT)

  private def isPrimitive(tpe: Z3Exp) = primitives.contains(tpe)

  private def isConcreteType(exp: Z3Exp, classTable: Map[String, ClassInfo]): Boolean = {
    val classes: Set[String] = classTable.keySet
    exp match {
      case Z3Type(className, typeArgs, _) => {
        isPrimitive(exp) ||
          (classes.contains(className) && typeArgs.forall(isConcreteType(_, classTable))) ||
          (className.contains("<") && className.contains(">")) // this is a type parameter that we declared, e.g. for a class Foo[V] we declare a new sort Foo<V> so Foo<V> is a "concrete type"
        // this is safe to do because only type parameters can contain "<" and ">" symbols (since they are not valid in class names)
      }
      case _ => false
    }
  }

  private def concretizeClassIfNeeded(methodName: String, clazz: Z3Type, methodTypeArgs: List[Z3Type]): State[CompilerState, Unit] =
    State.modify {
      state => {
        // Are all type arguments concrete?
        // e.g. class Foo(s: Set[Int]) is oke
        // but class Foo[A](s: Set[A]) not because A is still abstract so we cannot yet concretize Set
        val className = clazz.name
        val classTypeArgs = clazz.typeArgs

        if (classTypeArgs.forall(isConcreteType(_, state.classes))) {
          // all type arguments are concrete
          state.classes.get(className) match {
            case Some(ClassInfo(_, methods, _)) => {
              methods.get(methodName) match {
                case Some(MethodInfo(_, _, concretizer)) => {
                  // Fetch the set of concretized versions of this class' method
                  val methodVersions = state.concretizedMethods.getOrElse(className, Map.empty[String, Set[(List[Z3Type], List[Z3Type])]])
                  val concretizedVersions: Set[(List[Z3Type], List[Z3Type])] = methodVersions.getOrElse(methodName, Set.empty[(List[Z3Type], List[Z3Type])])
                  val classAndMethodTypeArgs = (classTypeArgs, methodTypeArgs)

                  if (!concretizedVersions.contains(classAndMethodTypeArgs)) {
                    // This version of the method (i.e. method + these type arguments) has not yet been concretized
                    // and the provided type arguments are all concrete (e.g. def foo(s: Set[Int]): Set[Int] is oke but, def foo(s: Set[V]): Set[V] not because V is still abstract so we cannot yet concretize `foo`
                    val newState = for {
                      _ <- State.modify[CompilerState] {
                        case s => {
                          val extendedConcretizedVersions = concretizedVersions + classAndMethodTypeArgs
                          val extendedMethodVersions = methodVersions + (methodName -> extendedConcretizedVersions)
                          s.copy(concretizedMethods = s.concretizedMethods + (className -> extendedMethodVersions)) // this state marks the fact that we are concretizing `<className> <..typeArgs>` such that we don't concretize it again (otherwise we end up in an infinite loop)
                        }
                      }
                      concretizedFunction <- concretizer(classTypeArgs, methodTypeArgs)
                      modifiedState <- State.modify[CompilerState](
                        s => s.copy(reversedProgram = concretizedFunction :: s.reversedProgram))
                    } yield modifiedState

                    // We need to return a state here (and not a state monad)
                    // So we need to run `newState` with the current state
                    // to get the new state and return that new state
                    newState.run(state).value._1
                  }
                  else
                    state
                }
                case None => state
              }
            }
            case None => state
          }
        }
        else {
          state
        }
      }
    }

  type Z3Code = String
  type ConcreteTypeArgs = List[Z3Type]
  type Function = Z3Code
  type MethodConcretizer = (ConcreteTypeArgs, ConcreteTypeArgs) => State[CompilerState, Function]

  final case class MethodInfo(name: String, method: Method, @transient concretizer: MethodConcretizer) {
    val arguments = method.args.map(arg => s"${arg.name}: ${arg.tpe}")
    val argumentNames = method.args.map(_.name)
    val returnType = method.retTpe.toString
  }

  final case class ClassInfo(name: String, methods: Map[String, MethodInfo], compiledClass: String) {
    def normalize() = {
      copy(methods = methods.map {
        case (methodName, info) => (z3FunNameToOriginalMethodName(methodName, name), info)
      })
    }

    def keepPublicMethodsOnly(): ClassInfo = copy(methods = methods.filter(nameAndMethod => {
      val mods = nameAndMethod._2.method.mods
      !(mods.contains(Private) || mods.contains(Protected))
    }))
  }

  final case class EnumCtorInfo(enumName: String, fields: List[Var], tparams: List[Z3Type]) {
    def fillTypeParameters(targs: List[Z3Type]): State[CompilerState, EnumCtorInfo] = {
      val substitutions = (tparams zip targs).toMap
      for {
        filledFields <- fields.traverse(_.fillTypeParameters(substitutions))
      } yield copy(fields = filledFields.asInstanceOf[List[Var]], tparams = targs)
    }
  }

  /**
   * @param classes Maps class names to a function that concretizes the class methods using the provided type parameters.
   * @param concretizedMethods Maps class name to a map that maps method name to a set of already concretized versions of that method (e.g. Set -> Map("add" -> Set[Int, String]))
   * @param enums Maps data constructor names to information about this data constructor.
   * @param reversedProgram Compiled program including class definitions, generic methods, and concretized methods in reversed order.
   */
  final case class CompilerState(classes: Map[String, ClassInfo] = Map(),
                                 concretizedMethods: Map[String, Map[String, Set[(List[Z3Type], List[Z3Type])]]] = Map(),
                                 enums: Map[String, EnumCtorInfo] = Map(),
                                 reversedProgram: List[String] = List())

  def makeTParamName(tpe: Z3Type): String = {
    tpe match {
      case Z3Type(name, targs, _) => {
        val params = targs.map(targ => s"<${makeTParamName(targ)}>").mkString
        s"$name$params"
      }
    }
  }

  def makeFunName(className: String, classTypeParams: List[Z3Type], opName: String, methodTypeParams: List[Z3Type]) = {
    val classNameWithTypeParams = makeTParamName(Z3Type(className, classTypeParams))
    val opNameWithTypeParams = makeTParamName(Z3Type(opName, methodTypeParams))
    s"${classNameWithTypeParams}_$opNameWithTypeParams"
  }

  def makeFieldName(fieldName: String, className: String) = s"$className>>$fieldName"

  /**
   * Computes the name of a class method based on its Z3 function name.
   * e.g. method `Foo[Int].test` in `class Foo[Int] { def test[A](a: A) = a }` has name `Foo<Int>_test<Foo_test<A>>` in Z3
   *      This function takes the name `Foo<Int>_test<Foo_test<A>>` and the class name `Foo` and returns the original method name `test`.
   */
  def z3FunNameToOriginalMethodName(z3FunName: String, className: String) = z3FunName.replaceAll(s"$className(\\<.*\\>)?_", "").replaceAll("(\\<.*\\>)?", "")
}

class Z3CompilerPlugin extends Plugin[Z3CompilerPlugin.Z3Exp, Z3CompilerPlugin.MethodOrPredicate, Z3CompilerPlugin.CompilerState] {
  import Z3CompilerPlugin._
  import be.vub.kdeporre.verifx.Utilities.Interpolators._

  val int_type: Z3Type = intT
  val bool_type: Z3Type = boolT
  val string_type: Z3Type = stringT

  val set_className: String = "Set"
  val map_className: String = "Map"
  val list_className: String = "List"

  private def makeComplexTypeInternal(name: String, compiledTypeArgs: List[Z3Exp], ogDataCtor: Option[Z3Type] = None) = {
    Z3Type(name, compiledTypeArgs.asInstanceOf[List[Z3Type]], ogDataCtor)
  }

  def makeComplexType(name: String, compiledTypeArgs: List[Z3Exp]): State[CompilerState, Z3Exp] = State.inspect {
    case s => {
      val tpe = makeComplexTypeInternal(name, compiledTypeArgs)

      s.enums.get(name) match {
        case Some(EnumCtorInfo(enumName, _, _)) => {
          // `name` is a constructor for `enumName` but is not a type on itself
          // only `enumName` is a valid type,
          // so replace `name` by `enumName`
          // but also store the original data constructor `tpe` that got replaced
          makeComplexTypeInternal(enumName, compiledTypeArgs, Some(tpe))
        }
        case _ => tpe
      }
    }
  }

  // Z3 does not support higher order types, e.g. Foo[A[_, _]]
  // So just translate the type parameter A[_, _] to A
  def makeTypeParam(name: String, tparams: List[Z3Exp], upperTypeBound: Option[Z3Exp]): State[CompilerState, Z3Exp] = State.pure(Z3Type(name, Nil))

  def makeLiteral(lit: Term.Literal): State[CompilerState, Z3Exp] = State.pure {
    lit match {
      case Term.BoolL(bool) => bool.toString
      case Term.IntL(value) => value.toString
      case Term.StringL(value) => s""""$value""""
    }
  }

  def makeBinaryOperation(op: Op.BinaryOperator, left: Z3Exp, right: Z3Exp): State[CompilerState, Z3Exp] = State.pure {
    val z3Operators: Map[Op.BinaryOperator, String] =
      Map(
        Op.Plus -> "+",
        Op.Minus -> "-",
        Op.Times -> "*",
        Op.Divide -> "/",
        Op.And -> "and",
        Op.Or -> "or",
        Op.Equals -> "=",
        Op.NotEquals -> "=",
        Op.SmallerThan -> "<",
        Op.SmallerOrEqual -> "<=",
        Op.BiggerThan -> ">",
        Op.BiggerOrEqual -> ">=")

    val z3Operator = z3Operators.get(op).get
    val binaryOp = BinaryOperation(z3Operator, left, right)

    op match {
      case Op.NotEquals => UnaryOperation("not", binaryOp)
      case _ => binaryOp
    }
  }

  def makeUnaryOperation(op: Op.UnaryOperator, arg: Z3Exp): State[CompilerState, Z3Exp] = State.pure {
    op match {
      case Op.Not => UnaryOperation("not", arg)
      case Op.Negative => BinaryOperation("*", arg, "-1")
    }
  }

  def makeReference(ref: String): State[CompilerState, Z3Exp] = State.pure(ref)

  def makeIf(condition: Z3Exp, thenb: Z3Exp, elseb: Z3Exp): State[CompilerState, Z3Exp] = State.pure(If(condition, thenb, elseb))

  private def classCtor(className: String) = s"mk-$className"

  def instantiateClass(className: String, tparams: List[Z3Exp], args: List[Z3Exp]): State[CompilerState, Z3Exp] = {
    // Concretize the class if needed
    makeComplexType(className, tparams) map {
      case tpe => {
        className match {
          case "Set" => {
            val List(elemType) = tparams.asInstanceOf[List[Z3Type]]
            val qualify = QualifiedIdentifier("const", Z3Type("Set", List(elemType)))
            Apply(qualify, List("false"))
          }
          case "Map" => {
            val List(keyType, valueType) = tparams.asInstanceOf[List[Z3Type]]
            val qualify = QualifiedIdentifier("const", Z3Type("Map", List(keyType, valueType)))
            val qualifiedNone = QualifiedIdentifier("None", Z3Type("Option", List(valueType)))
            Apply(qualify, List(qualifiedNone))
          }
          case _ => {
            // user-defined class
            val ctor = classCtor(className)
            val qualifiedCtor = QualifiedIdentifier(ctor, tpe)
            Apply(qualifiedCtor, args)
          }
        }
      }
    }
  }

  def makeFieldAccess(classType: Z3Exp, obj: Z3Exp, field: String, fieldTpe: Z3Exp): State[CompilerState, Z3Exp] = State.pure {
    // `obj` may be an enumeration in which case the `classType` will be the type of the enumeration
    // but we need the name of the data constructor to construct the name of the field
    // therefore, we call `getDataCtorTpe` to get the type of the data constructor
    // (if `obj` is not an enumeration `getDataCtorTpe` will leave `classType` unchanged)
    val className = classType.asInstanceOf[Z3Type].getDataCtorTpe().name
    val fieldName = makeFieldName(field, className)
    Apply(QualifiedIdentifier(fieldName, fieldTpe), List(obj))
  }

  def makeMethodReference(classType: Z3Exp, obj: Z3Exp, method: String, signature: Z3Exp): State[CompilerState, Z3Exp] = {
    // `obj.method` needs to be transformed to a lambda `(arg1, ..., argN) => obj.method(arg1, ..., argN)`

    val tpe = signature.asInstanceOf[Z3Type]
    // Turn the method reference into a lambda
    val paramTypes = tpe.typeArgs.dropRight(1) // `tpe.typeArgs` is the type signature: Arg1 Arg2 ... ArgN RetTpe, so we drop the return type
    var ctr = 0
    val params = paramTypes.map(tpe => {
      ctr = ctr + 1
      Plugin.Arg[Z3Exp](s"arg$ctr", tpe)
    })

    for {
      body <- makeMethodCall(classType, obj, method, Nil, params.map(_.name), params.map(_.tpe)) // method reference is only allowed to methods that do not have type parameters
      fn <- makeFunction(params, body)
    } yield fn
  }

  def makeMethodCall(classType: Z3Exp, obj: Z3Exp, method: String, targs: List[Z3Exp], args: List[Z3Exp], argsTypes: List[Z3Exp]): State[CompilerState, Z3Exp] = {
    val clazz = classType.asInstanceOf[Z3Type]
    val className = clazz.name
    val argTypes = argsTypes.asInstanceOf[List[Z3Type]]

    if (method == "asInstanceOf")
      State.pure(obj) // ignore `asInstanceOf` calls, because those are only allowed to cast an enumeration type to a data constructor type, but in Z3 data constructors are of the enumeration type
    else {
      className match {
        case "Set" => {
          val valueType = clazz.typeArgs.head

          def contains(elem: Z3Exp) = Select(obj, elem)
          def mapFunction(fun: Z3Exp, forall: Boolean = true) = {
            /*
              s.forall(f: V => Boolean) =
                (forall ((v $valueType))
                  if (s.contains("v"))
                    f("k", "v")
                  else
                    true  <---------

              s.exists(f: V => Boolean) =
                (exists ((v $valueType))
                  if (m.contains("v"))
                    f("k", "v")
                  else
                    false  <---------
             */

            val quantifier = if (forall) "forall" else "exists"
            val ifNotPresent = if (forall) "true" else "false"

            Quantifier(
              quantifier,
              List(Var("v<>", valueType)), // we use the <> to avoid name clashes with names in user-defined code
              If(
                contains("v<>"),
                apply(fun, "v<>"),
                ifNotPresent))
          }
          def filter(set: Z3Exp, fun: Z3Exp, valueType: Z3Type) = {
            Lambda(
              List(Var("e<>", valueType)),
              And(contains("e<>"), Select(fun, "e<>")))
          }
          def mapTransformer() = {
            // Maps a function over the set
            // set.map[W](f: V => W) = lambda (w) set.exists(v => f(v) == w)
            val methodTypeArgs = targs.asInstanceOf[List[Z3Type]]
            val newValueType = methodTypeArgs.head
            val f = args.head
            val v = "vv<>"
            val w = "w<>"

            // v => f(v) == w
            val transformFn = Lambda(List(Var(v, valueType)), Equals(apply(f, v), w))

            // lambda (w: W) { set.exists(v => f(v) == w) }
            Lambda(
              List(Var(w, newValueType)),
              mapFunction(transformFn, false))
          }

          State.pure {
            method match {
              case "add" => Apply("store", List(obj, args.head, "true"))
              case "remove" => Apply("store", List(obj, args.head, "false"))
              case "contains" => contains(args.head)
              case "union" => {
                val union = QualifiedIdentifier("union", classType)
                Apply(union, List(obj, args.head))
              }
              case "intersect" => Apply("intersection", List(obj, args.head))
              case "diff" => Apply("setminus", List(obj, args.head))
              case "subsetOf" => Apply("subset", List(obj, args.head))
              case "map" => mapTransformer()
              case "filter" => filter(obj, args.head, valueType)
              case "forall" => mapFunction(args.head)
              case "exists" => mapFunction(args.head, false)
              case "isEmpty" => Quantifier("forall", List(Var("e<>", valueType)), UnaryOperation("not", contains("e<>")))
              case "nonEmpty" => Quantifier("exists", List(Var("e<>", valueType)), contains("e<>"))
              case _ => throw new Error(s"Method not found: Set.$method")
            }
          }
        }
        case "Map" => {
          val List(keyType, valueType) = clazz.typeArgs
          val maybeValueType = Z3Type("Option", List(valueType))

          def contains(key: Z3Exp) = {
            // The below corresponds to this: s"(not (= (select $obj $key) (as None (Option $valueType))))"
            Apply(
              "not",
              List(
                Apply(
                  "=",
                  List(
                    Select(obj, key),
                    QualifiedIdentifier("None", maybeValueType)))))
          }
          def get(key: Z3Exp) = Get(Select(obj, key), valueType) // we may be applying `(get None)`, user code should always first check that the key is contained in the map before calling get!
          def mapFunction(fun: Z3Exp, forall: Boolean = true) = {
            /*

                 m.forall(f: (K, V) => Boolean) =
                   (forall ((k $keyType) (v $valueType))
                     if (m.contains("k") && m.get("k") == v)
                       f("k", "v")
                     else
                       true  <--------

                 m.exists(f: (K, V) => Boolean) =
                   (exists ((k $keyType) (v $valueType))
                     if (m.contains("k") && m.get("k") == v)
                       f("k", "v")
                     else
                       false  <--------
             */
            val quantifier = if (forall) "forall" else "exists"
            val ifNotPresent = if (forall) "true" else "false"

            Quantifier(
              quantifier,
              List(Var("k<>", keyType), Var("v<>", valueType)), // we use <> to avoid name clashes with user-defined code
              If(
                And(contains("k<>"), Equals(get("k<>"), "v<>")),
                apply(fun, "k<>", "v<>"),
                ifNotPresent))
          }

          State.pure {
            method match {
              case "add" => {
                val List(key, value) = args
                Apply("store", List(obj, key, Apply(QualifiedIdentifier("Some", Z3Type("Option", List(valueType))), List(value))))
              }
              case "remove" => {
                val List(key) = args
                Apply("store", List(obj, key, QualifiedIdentifier("None", Z3Type("Option", List(valueType)))))
              }
              case "get" => {
                val List(key) = args
                get(key)
              }
              case "contains" => {
                val List(key) = args
                contains(key)
              }
              case "getOrElse" => {
                val List(key, default) = args
                // Make the following condition: s"(not (= (select $obj $key) (as None (Option $valueType))))"
                val keyIsPresent =
                  Not(Equals("maybeValue", QualifiedIdentifier("None", maybeValueType)))

                Let(
                  List(Var("maybeValue", Select(obj, key))),
                  If(
                    keyIsPresent,
                    Get("maybeValue", valueType),
                    default))
              }
              case "keys" => {
                /*
                 * m.keys() =
                 *   (lambda ((key KeyType))
                 *     contains(key))
                 */

                Lambda(
                  List(Var("key<>", keyType)),
                  contains("key<>"))
              }
              case "values" => {
                /*
                 * m.values() =
                 *   (lambda ((v ValueType))
                 *     (exists ((key KeyType))
                 *       m.get(key) == Some(v)))
                 */

                Lambda(
                  List(Var("v<>", valueType)),
                  Exists(
                    List(Var("key<>", keyType)),
                    Equals(
                      Select(obj, "key<>"),
                      Somee("v<>", valueType))))
              }
              case "bijective" => {
                /*
                 * Returns true if the mapping is bijective,
                 * i.e. if the mapping is one-to-one and onto
                 *      onto = every value is mapped by a key -> this is always the case for a map
                 *      one-to-one = no two keys map to the same value
                 *
                 * m.bijective() =
                 *   (forall ((key1 KeyType) (key2 KeyType))
                 *     (key1 != key2 && m.contains(key1) && m.contains(key2)) => {
                 *       m.get(key1) != m.get(key2)))
                 *     })
                 */

                Forall(
                  List(Var("k1<>", keyType), Var("k2<>", keyType)),
                  Implies(
                    And(
                      Not(Equals("k1<>", "k2<>")),
                      And(
                        contains("k1<>"),
                        contains("k2<>"))),
                    Not(Equals(Select(obj, "k1<>"), Select(obj, "k2<>")))))
              }
              case "toSet" => {
                /*
                 * m.toSet() = (lambda ((tuple Tuple[K, V]))
                 *                m.contains(tuple._1) && map.get(tuple._1) == tuple._2)
                 */

                val kvTupleType = Z3Type("Tuple", List(keyType, valueType))
                val key = Apply(QualifiedIdentifier("Tuple>>fst", keyType), List("kvTuple<>"))
                val value = Apply(QualifiedIdentifier("Tuple>>snd", valueType), List("kvTuple<>"))

                Lambda(
                  List(Var("kvTuple<>", kvTupleType)),
                  Let(
                    List(Var("key<>", key)),
                    And(
                      contains("key<>"),
                      Equals(get("key<>"), value))))
              }
              case "filter" => {
                /*
                 * m.filter(f) =
                 *   (lambda ((key KeyType))
                 *     if (contains(key) && f(key, m.get(key)))
                 *       Some(m.get(key))
                 *     else
                 *       None)
                 */

                val fun = args.head

                Lambda(
                  List(Var("key<>", keyType)),
                  Let(
                    List(Var("value<>", get("key<>"))),
                    If(
                      And(contains("key<>"), Select(fun, "key<>", "value<>")),
                      Somee("value<>", valueType),
                      Nonee(valueType))))
              }
              case "map" | "mapValues" => {
                /* To map a function over a map (i.e. an array) we need to make a lambda
                 because lambda's are syntactic sugar for arrays from the argument types to the return type

                obj.map(fun) =
                  (lambda ((key KeyType))
                    (let ((maybeValue (select obj key)))
                      ; Is the key in the map?
                      (ite (= maybeValue (as None (Option ValueType)))
                        (as None (Option NewValueType)) ; No
                        ; Yes, map the function over the entry
                        ((as Some (Option NewValueType))
                          (select
                            fun                   ; function to map
                            key                   ; pass the key to the function
                            (get maybeValue)))))) ; also pass the value to the function

                fun is a function (KeyType, ValueType) => ResultType
                NOTE: map returns a new map with the values modified by applying `fun` on them
                      However, we can't use map to add entries or remove entries from the map.
               */

                val List(fun) = args
                val List(funType) = argTypes
                val newValueType = funType.typeArgs.last // return type of the function
                val maybeNewValueType = Z3Type("Option", List(newValueType))

                val maybeValueType = Z3Type("Option", List(valueType))
                val none = QualifiedIdentifier("None", maybeValueType)
                val noneAsNewValueType = QualifiedIdentifier("None", maybeNewValueType)

                val callFun =
                  if (method == "map")
                    Select(fun, "key<>", Get("maybeValue<>", valueType)) // call `fun(key, value)`
                  else
                    Select(fun, Get("maybeValue<>", valueType)) // call `fun(value)`

                Lambda(
                  List(Var("key<>", keyType)),
                  Let(
                    List(Var("maybeValue<>", Select(obj, "key<>"))),
                    If(
                      Equals("maybeValue<>", none),
                      noneAsNewValueType,
                      Apply(
                        QualifiedIdentifier("Some", maybeNewValueType),
                        List(callFun)))))
              }
              case "zip" => {
                /*
                 * To zip 2 maps (i.e. arrays) we make a lambda that maps keys to a tuple of the values it fetches in both maps.
                 * m1 is of type Map[KeyType, ValueType]
                 * m2 is of type Map[KeyType, Value2Type]
                 *
                 * m1.zip(m2) =
                 *   (lambda ((key KeyType))
                 *     (let ((maybeValue1 (select m1 key))
                 *           (maybeValue2 (select m2 key)))
                 *       ; Is the key in both maps?
                 *       (ite (or
                 *              (= maybeValue1 (as None (Option ValueType)))
                 *              (= maybeValue2 (as None (Option Value2Type))))
                 *         (as None (Option (Tuple ValueType Value2Type))) ; No
                 *         ; Yes, make a tuple containing the 2 values
                 *         ((as Some (Option (Tuple ValueType Value2Type))) ((as mk-Tuple (Tuple ValueType Value2Type)) (get maybeValue1) (get maybeValue2))))))
                 *
                 */

                argTypes.head match {
                  case Z3Type("Map", List(_, value2Type), _) => {
                    val m1 = obj
                    val List(m2) = args
                    val maybeValueType = Z3Type("Option", List(valueType))
                    val maybeValue2Type = Z3Type("Option", List(value2Type))
                    val noneAsMaybeValue = QualifiedIdentifier("None", maybeValueType)
                    val noneAsMaybeValue2 = QualifiedIdentifier("None", maybeValue2Type)
                    val tupleType = Z3Type("Tuple", List(valueType, value2Type))
                    val maybeTupleType = Z3Type("Option", List(tupleType))
                    val noneAsMaybeTupleType = QualifiedIdentifier("None", maybeTupleType)
                    val qualifiedSomeTupleValue = QualifiedIdentifier("Some", maybeTupleType)
                    val makeTuple = QualifiedIdentifier("mk-Tuple", tupleType)

                    Lambda(
                      List(Var("key<>", keyType)),
                      Let(
                        List(
                          Var("maybeValue1<>", Select(m1, "key<>")),
                          Var("maybeValue2<>", Select(m2, "key<>"))),
                        If(
                          Or(
                            Equals("maybeValue1<>", noneAsMaybeValue),
                            Equals("maybeValue2<>", noneAsMaybeValue2)),
                          noneAsMaybeTupleType,
                          Apply(qualifiedSomeTupleValue, List(Apply(makeTuple, List(Get("maybeValue1<>", valueType), Get("maybeValue2<>", value2Type)))))
                        )
                      )
                    )
                  }
                }
              }
              case "combine" => {
                /*
                 * Combines two maps into one.
                 * If a key occurs in both maps, the values are combined using the provided function.
                 * If a key occurs in only one map, its value is copied to the resulting map.
                 * Hence, `combine` differs from `zip` in that the resulting map contains all keys whereas with `zip` it only contains the common keys.
                 *
                 * m1.combine(m2, fun) =
                 *   (lambda ((key KeyType))
                 *     (let ((maybeValue1 (select m1 key))
                 *           (maybeValue2 (select m2 key)))
                 *       ; Is the key absent in m1?
                 *       (ite (= maybeValue1 (as None (Option ValueType)))
                 *         maybeValue2 ; key is not in m1, return maybeValue2
                 *         ; Is the key absent in m2?
                 *         (ite (= maybeValue2 (as None (Option ValueType)))
                 *           maybeValue1 ; key is not in m2, but is in m1
                 *           ((as Some (Option ValueType))
                 *            (select
                 *              fun                     ; apply the function on both values
                 *              (get maybeValue1)
                 *              (get maybeValue2))))))) ; key is in both m1 and m2
                 */

                val m1 = obj
                val List(m2, fun) = args
                val noneAsMaybeValue = QualifiedIdentifier("None", maybeValueType)
                val qualifiedSomeValue = QualifiedIdentifier("Some", maybeValueType)

                Lambda(
                  List(Var("key<>", keyType)),
                  Let(
                    List(
                      Var("maybeValue1<>", Select(m1, "key<>")),
                      Var("maybeValue2<>", Select(m2, "key<>"))),
                    If(
                      Equals("maybeValue1<>", noneAsMaybeValue),
                      "maybeValue2<>",
                      If(
                        Equals("maybeValue2<>", noneAsMaybeValue),
                        "maybeValue1<>",
                        Apply(qualifiedSomeValue, List(Select(fun, Get("maybeValue1<>", valueType), Get("maybeValue2<>", valueType))))))))
              }
              case "forall" => mapFunction(args.head)
              case "exists" => mapFunction(args.head, false)
              case _ => throw new Error(s"Method not found: Map.$method")
            }
          }
        }
        case _ => {
          // user-defined class
          // concretize the class since we need to call the method on it
          val methodTypeArgs = targs.asInstanceOf[List[Z3Type]]
          concretizeClassIfNeeded(method, clazz, methodTypeArgs) map {
            case _ => {
              val classTypeParams = clazz.typeArgs
              val methodName = MethodName(className, classTypeParams, method, methodTypeArgs)
              val extendedArgs = if (ProgramCompiler.isProofClass(className)) args else obj :: args // methods in proof classes don't take a `this` argument because proof classes don't have fields
              MethodCall(clazz, methodName, methodTypeArgs, extendedArgs)
            }
          }
        }
      }
    }
  }

  def makeValDef(name: String, tpe: Z3Exp, value: Z3Exp): State[CompilerState, Z3Exp] = State.pure(ValDef(name, tpe, value))
  def makeValDecl(name: String, tpe: Z3Exp): State[CompilerState, Z3Exp] = State.pure(Z3Wrapper("")) // ignore value declarations

  def makeBlock(block: List[Z3Exp]): State[CompilerState, Z3Exp] = State.pure {
    // A block can only contain value definitions and terms
    // However, since there are no side effects only the last term is important.
    // e.g.:
    //  {
    //     val x = 2
    //     val y = 3
    //     x+y // not important
    //     val z = 1
    //     x+y+Z // important

    val (stats, last) = block.splitAt(block.length - 1)
    val body = last.head
    val valDefs = stats.filter(_.isInstanceOf[ValDef]).asInstanceOf[List[ValDef]] // keep only value definitions
    val bindings = valDefs.map { case ValDef(name, _, value) => Var(name, value) }

    // Transform the value definitions into nested lets
    bindings.reverse.foldLeft(body) {
      case (body, binding) =>
        Quantifier("let", List(binding), body)
    }
  }

  def makeMethodDecl(mods: List[Z3Exp], name: String, tparams: List[Z3Exp], args: List[Plugin.Arg[Z3Exp]], retTpe: Z3Exp): State[CompilerState, Z3Exp] = State.pure(Z3Wrapper(""))

  // A function (i.e. lambda) is an array
  // e.g. (x: A, y: B) => someValueOfTypeC will have type (Array A B C)
  def makeFunctionType(paramTypes: List[Z3Exp], resType: Z3Exp): State[CompilerState, Z3Exp] = makeComplexType("Array", paramTypes.appended(resType))

  def makeFunction(params: List[Plugin.Arg[Z3Exp]], body: Z3Exp): State[CompilerState, Z3Exp] = State.pure {
    val vars = params.map { case Plugin.Arg(name, tpe) => Var(name, tpe) }
    Quantifier("lambda", vars, body)
  }

  // Applying a function corresponds to reading the array using the provided arguments
  // e.g. f(1, "b") corresponds to (select f 1 "b")
  private def apply(fun: Z3Exp, args: Z3Exp*) = {
    Apply("select", fun :: args.toList)
  }

  def makeFunctionCall(fun: Z3Exp, args: List[Z3Exp]): State[CompilerState, Z3Exp] = State.pure(apply(fun, args:_*))

  def makeMod(mod: IR.Mod.Mod): State[CompilerState, Z3Exp] = State.pure {
    mod match {
      case IR.Mod.Private => Private
      case IR.Mod.Protected => Protected
      case IR.Mod.Sealed => Sealed
      case _ => Z3Wrapper("") // ignore all other modifiers
    }
  }

  def makeMethodDef(name: String, mods: List[Z3Exp], tparams: List[Z3Exp], args: List[Plugin.Arg[Z3Exp]], body: Z3Exp, retTpe: Z3Exp, isRecursive: Boolean): State[CompilerState, MethodOrPredicate] = {
    State.pure(MethodWithoutSpec(mods, name, tparams.asInstanceOf[List[Z3Type]], args, body, retTpe.asInstanceOf[Z3Type], isRecursive))
  }

  def makeContextDef     (name: String, tparams: List[Z3Exp], args: List[Plugin.Arg[Z3Exp]], body: Z3Exp): State[CompilerState, MethodOrPredicate] = State.pure(Ctx(name, args, body))
  def makePreconditionDef(name: String, tparams: List[Z3Exp], args: List[Plugin.Arg[Z3Exp]], body: Z3Exp): State[CompilerState, MethodOrPredicate] = State.pure(Pre(name, args, body))
  def makeInvariantDef   (name: String, tparams: List[Z3Exp], args: List[Plugin.Arg[Z3Exp]], body: Z3Exp): State[CompilerState, MethodOrPredicate] = State.pure(Inv(name, args, body))

  private def groupMethodsAndPreds(xs: List[MethodOrPredicate]): (List[Method], Map[String, Method]) = {
    // Group methods and predicates by method name
    val groupedStats = xs.groupMap(_.name)(identity)

    // Sort the groups according to their definition order in `stats`
    val methodDefOrder = xs.filter(_.isInstanceOf[MethodWithoutSpec]).map(_.name)
    val groups = SortedMap.from(groupedStats)(Ordering.by((s: String) => methodDefOrder.indexOf(s)))

    // Transform the list of groups into a list of methods
    val methods = groups.values map {
      case methodsAndPreds => {
        methodsAndPreds.find(_.isInstanceOf[MethodWithoutSpec]) match {
          case None => throw new Error(s"Missing method definition for ${methodsAndPreds.head.name}")
          case Some(m) => {
            val methodWithoutSpec = m.asInstanceOf[MethodWithoutSpec]
            val ctx = methodsAndPreds.find(_.isInstanceOf[Ctx]).asInstanceOf[Option[Ctx]]
            val pre = methodsAndPreds.find(_.isInstanceOf[Pre]).asInstanceOf[Option[Pre]]
            val inv = methodsAndPreds.find(_.isInstanceOf[Inv]).asInstanceOf[Option[Inv]]
            (m.name, methodWithoutSpec.extend(ctx, pre, inv))
          }
        }
      }
    }

    val methodsList = methods.map(_._2).toList
    val methodsMap = methods.toMap
    (methodsList, methodsMap)
  }

  def makeClassDef(annots: List[String], className: String, compiledTparams: List[Z3Exp], fs: List[Plugin.Field[Z3Exp]], mAndPs: List[MethodOrPredicate], methodsAndPreds: List[MethodOrPredicate], supert: Option[Z3Exp]): State[CompilerState, Z3Exp] = {
    val (methodsList, methods) = groupMethodsAndPreds(methodsAndPreds)

    val tparams = compiledTparams.asInstanceOf[List[Z3Type]]

    val ctor = classCtor(className)

    makeComplexType(className, tparams) flatMap {
      classType => {
        val fields = fs.map(f => f.copy(name = makeFieldName(f.name, className)))
        val fieldLst = fields.map(f => s"(${f.name} ${f.tpe})").mkString(" ")

        def makeMethodTParamName(className: String, opName: String, typeParam: String) = s"${className}_$opName<$typeParam>"

        // Declare this Class using Z3's record types
        // Also define the class methods as functions from Instance + Args to a new Instance
        // Also define the class methods' context, precondition, and invariant as functions

        def concretizeFilledMethod(m: MethodWithFullName, concreteTParams: List[Z3Type], newClassType: Z3Exp) = m match {
          case MethodWithFullName(methodNameObj, Method(_, _, _, args, body, retTpe, isRecursive, context, precondition, invariant)) => {
            // In the body of this method also replace the type parameters in function names
            val self = Plugin.Arg("this", newClassType)
            val old = Plugin.Arg("old", newClassType)
            val methodName = methodNameObj.toString
            val methodArgs = if (ProgramCompiler.isProofClass(className)) args else self :: args // do not add "this" argument to proofs
            val methodFun = makeFunction(methodName, methodArgs, body, retTpe, isRecursive)

            // Turn the method into Z3 code
            methodFun.toString()
          }
        }

        // Concretizes all methods of this class
        def concretizeClass(concreteTParams: List[Z3Type]) = {
          // Substitute the given types and also substitute this class type by the concrete class type, e.g. Set<A> to Set<Int>
          makeComplexType(className, concreteTParams) flatMap {
            newClassType => {
              val classSubstitutionList = (classType.asInstanceOf[Z3Type], newClassType.asInstanceOf[Z3Type]) :: (tparams zip concreteTParams)
              val classSubstitutions = classSubstitutionList.toMap
              for {
                // Subtitute the type params in the fields and concretize types where needed
                _ <- fields.traverse(_.fillTypeParameters(classSubstitutions))
                // Substitute the class type params AND the method type params in the methods and concretize the methods
                ms <- methodsList.traverse(method => {
                  val methodGenericTParamNames = method.tparams.map(tparam => Z3Type(makeMethodTParamName(className, method.name, tparam.name), Nil))
                  val methodSubstitutions = method.tparams zip methodGenericTParamNames
                  val methodAndClassSubstitutions = (methodSubstitutions ++ classSubstitutionList).distinctBy(_._1).toMap

                  for {
                    // Store the fact that we are concretizing this method
                    // such that later it won't be concretized again
                    _ <- State.modify[CompilerState] {
                      case s => {
                        val methodName = method.name
                        val methodVersions = s.concretizedMethods.getOrElse(className, Map.empty[String, Set[(List[Z3Type], List[Z3Type])]])
                        val concretizedVersions: Set[(List[Z3Type], List[Z3Type])] = methodVersions.getOrElse(methodName, Set.empty[(List[Z3Type], List[Z3Type])])
                        val classAndMethodTypeArgs = (concreteTParams, methodGenericTParamNames)

                        val extendedConcretizedVersions = concretizedVersions + classAndMethodTypeArgs
                        val extendedMethodVersions = methodVersions + (methodName -> extendedConcretizedVersions)
                        s.copy(concretizedMethods = s.concretizedMethods + (className -> extendedMethodVersions))
                      }
                    }
                    // Fill the methods (i.e. concretize them)
                    filledMethod <- method.fillTypeParameters(methodAndClassSubstitutions)
                  } yield filledMethod
                }) map {
                  newMethods => {
                    // Now for each of those methods, make a Z3 function
                    val newMethodsWithFullName = newMethods.asInstanceOf[List[Method]].map(m => m.setFullName(className, concreteTParams))
                    val compiledMethods = newMethodsWithFullName.map(concretizeFilledMethod(_, concreteTParams, newClassType))
                    (compiledMethods, newMethodsWithFullName)
                  }
                }
              } yield ms
            }
          }
        }

        def concretizeMethod(methodName: String, concreteClassTParams: List[Z3Type], concreteMethodTParams: List[Z3Type]): State[CompilerState, Z3Code] = {
          // Substitute the given types and also substitute this class type by the concrete class type, e.g. Set<A> to Set<Int>
          makeComplexType(className, concreteClassTParams) flatMap {
            newClassType => {
              methods.get(methodName) match {
                case Some(method) => {
                  // Substitute all type parameters
                  // note that method type parameters may shadow class type parameters!
                  val classTParams = tparams
                  val methodTParams = method.tparams
                  val allTParams = (methodTParams zip concreteMethodTParams) ++ (classTParams zip concreteClassTParams)
                  val substitutions = allTParams.distinctBy(_._1).toMap

                  method
                    .fillTypeParameters(substitutions)
                    .map(_.asInstanceOf[Method].setFullName(className, concreteClassTParams))
                    .map(m => concretizeFilledMethod(m, concreteClassTParams, newClassType))
                }
                case None => throw new InternalError(s"Method $methodName does not exist in class $className")
              }
            }
          }
        }

        val concreteTParamsType = tparams.map(tparam => Z3Type(className, List(tparam)))
        val concreteTParams = concreteTParamsType.map(makeTParamName)

        // Now declare a sort and use it to define a "generic" version of the method
        // which we can use to analyze this generic class
        // so for every class type parameter we declare it as a sort and fill those sorts in as the type parameters of this class in the functions
        // e.g. class Map[K, V]() { def add(key: K, value: V) = ... }
        //      then we declare 2 sorts: Map<K> and Map<V>
        //      and concretize the `add` function: def add(key: Map<K>, value: Map<V>) = ... (but then in Z3 syntax ofcourse)
        //      which will allow analysis of this generic class
        val classTParamDeclarations = concreteTParams.map(tparam => s"(declare-sort $tparam)").mkString("\n")
        val classTParamsTypes = concreteTParams.map(Z3Type(_, Nil)) // these will be the generic types, e.g. Map<K> instead of (Map K)

        // Also declare a sort for every type parameter of the method
        val methodConcreteTParams = methodsList.map(m => m.tparams.map(tparam => makeMethodTParamName(className, m.name, tparam.name))).flatten
        val methodTParamDeclarations = methodConcreteTParams.map(tparam => s"(declare-sort $tparam)").mkString("\n")

        // Update the state to store the `concretizeMethod` function such that we can later concretize the methods
        // when we e.g. encounter a method call `obj.method[Int, String](...)` --> then we will replace the type params by Int and String and define those functions

        for {
          // First declare the type parameters
          _ <- State.modify[CompilerState](
            s => s.copy(reversedProgram = methodTParamDeclarations :: classTParamDeclarations :: s.reversedProgram))
          // Then add the class information to the state
          _ <- State.modify[CompilerState] {
            case s => {
              val datatypeDef = s"(declare-datatypes (${tparams.mkString(" ")}) (($className ($ctor $fieldLst))))"
              val classInfo = ClassInfo(className, methodsList.map(m => (m.name, MethodInfo(m.name, m, concretizeMethod(m.name, _, _)))).toMap, datatypeDef)
              s.copy(classes = s.classes + (className -> classInfo), reversedProgram = datatypeDef :: s.reversedProgram)
            }
          }
          // Then concretize this class (i.e. concretize the fields and methods by replacing the type parameters by the concrete types we declared for those type parameters)
          // The fields/methods may use a type parameter which triggers the concretization of another class
          // that's why we have to declare the type parameters before concretizing the class
          genericMethods <- concretizeClass(classTParamsTypes)
          res <- State[CompilerState, Z3Exp] {
            s => {
              val genericMethodsStr = genericMethods._1.mkString("\n\n")
              val compiledClass =
                if (!ProgramCompiler.isProofClass(className))
                  ind"""(declare-datatypes (${tparams.mkString(" ")}) (($className ($ctor $fieldLst))))
                       |$genericMethodsStr""".stripMargin
                else {
                  // No need to declare the data type if it is a proof because it is a dummy data type anyway
                  genericMethodsStr
                }

              val classInfo = ClassInfo(className, genericMethods._2.map(m => (m.method.name, MethodInfo(m.methodName.toString, m.method, concretizeMethod(m.method.name, _, _)))).toMap, compiledClass)
              val newState = s.copy(classes = s.classes + (className -> classInfo), reversedProgram = genericMethodsStr :: s.reversedProgram) // update this class' information

              (newState, compiledClass)
            }
          }
        } yield res

      }
    }
  }

  def makeTraitDef(mods: List[Z3Exp], name: String, tparams: List[Z3Exp], stats: List[Z3Exp], supert: Option[Z3Exp]): State[CompilerState, Z3Exp] = State.pure("")

  def makeEnumDef(enumName: String, tparams: List[Z3Exp], ctors: List[Plugin.EnumCtor[Z3Exp]]): State[CompilerState, Z3Exp] = {
    val tparamsStr = tparams.mkString(" ")
    val ctorsStr = ctors.map {
      case Plugin.EnumCtor(ctorName, _, fields) => {
        if (fields.isEmpty)
          classCtor(ctorName)
        else {
          val fs = fields.map(f => f.copy(name = makeFieldName(f.name, ctorName)))
          val fieldLst = fs.map(f => s"(${f.name} ${f.tpe})").mkString(" ")
          val ctor = classCtor(ctorName)
          s"($ctor $fieldLst)"
        }
      }
    }.mkString(" ")

    val enumDef = s"(declare-datatypes ($tparamsStr) (($enumName $ctorsStr)))"

    State[CompilerState, Z3Exp] {
      s => {
        val extendedEnums = s.enums ++ ctors.map(ctor => {
          val ctorFields: List[Var] = ctor.fields.map(f => Var(f.name, f.tpe))
          val ctorInfo = EnumCtorInfo(enumName, ctorFields, tparams.asInstanceOf[List[Z3Type]])
          (ctor.name, ctorInfo)
        })
        (s.copy(enums = extendedEnums,  reversedProgram = enumDef :: s.reversedProgram), enumDef)
      }
    }
  }

  private def getConcreteDataCtor(ctorTpe: Z3Type): State[CompilerState, EnumCtorInfo] = for {
    ctorInfo <- State.inspect[CompilerState, EnumCtorInfo] {
      case state => {
        val ctorName = ctorTpe.name
        state.enums.get(ctorName) match {
          case Some(enumCtorInfo) => enumCtorInfo
          case None => throw new InternalError(s"Class $ctorName not found.")
        }
      }
    }
    filledCtorInfo <- ctorInfo.fillTypeParameters(ctorTpe.typeArgs)
  } yield filledCtorInfo

  private def compileCase(exp: Z3Exp, casee: Plugin.Case[Z3Exp]): State[CompilerState, Z3Exp] = casee match {
    // case name: Type => body
    case Plugin.Case(Plugin.Pat.Typed(Plugin.Pat.Var(name), tpe), body) => {
      // ((Type _ _ ... _) (let ((name exp)) body))

      // Fetch the ADT and see how many fields this constructor has and replace each field by _
      val ctorTpe = tpe.asInstanceOf[Z3Type].ogDataCtorTpe.get
      //val ctorTpe = getCaseType(casee).get.name

      for {
        concreteCtorInfo <- getConcreteDataCtor(ctorTpe)
        vars <- State.pure(concreteCtorInfo.fields.map(_ => "_"))
      } yield Pattern(CtorCase(ctorTpe.name, vars), Let(List(Var(name, exp)), body))
    }
    // case _: Type => body
    case Plugin.Case(Plugin.Pat.Typed(Plugin.Pat.Wildcard, tpe), body) => {
      // ((Type _ _ ... _) body)

      // Fetch the ADT and see how many fields this constructor has and replace each field by _
      val ctorTpe = tpe.asInstanceOf[Z3Type].ogDataCtorTpe.get
      //val ctorTpe = getCaseType(casee).get.name

      for {
        concreteCtorInfo <- getConcreteDataCtor(ctorTpe)
        vars <- State.pure(concreteCtorInfo.fields.map(_ => "_"))
      } yield Pattern(CtorCase(ctorTpe.name, vars), body)
    }
    // case _ => body
    case Plugin.Case(Plugin.Pat.Wildcard, body) => State.pure {
      // (_ body)
      Pattern(VariableCase("_"), body)
    }
    case Plugin.Case(Plugin.Pat.Var(name), body) => State.pure {
      // (name body)
      Pattern(VariableCase(name), body)
    }
    // case Ctor(var1 ... var_n) => body
    case casee @ Plugin.Case(Plugin.Pat.Extract(tpe, vars), body) => State.pure {
      // ((Ctor var1 ... var_n) body)
      val ctorTpe = tpe.asInstanceOf[Z3Type].ogDataCtorTpe.get.name
      //val ctorTpe = getCaseType(casee).get.name

      val varsStr = vars.map {
        case Plugin.Pat.Var(name) => name
        case Plugin.Pat.Wildcard => "_"
      }

      Pattern(CtorCase(ctorTpe, varsStr), body)
    }
  }

  /**
   * Compiles a pattern match expression into nested if tests.
   * For example, assume we have an enumeration type E with 3 constructors of type C1, C2, and C3:
   * {{{
   * enum E {
   *   C1(a: Int, b: String) | C2(a: Int) | C3()
   * }
   * }}}
   *
   * Now, we can pattern match on expressions of type `E`.
   * For example, if `exp` is of type `E`, we can pattern match as follows:
   *
   * {{{
   * exp match {
   *   case i1: C1 => b1
   *   case i2: C2 => b2
   *   case i3: C3 => b3
   * }
   * }}}
   *
   * and this will be compiled into a pattern match expression:
   *
   * {{{
   *   (match exp (
   *     ((C1 _ _) (let ((i1 exp)) b1))
   *     ((C2 _) (let ((i2 exp)) b2))
   *     (C3 (let ((i3 exp)) b3))))
   * }}}
   *
   * (or just replace every occurence of `i1`/`i2`/`i3` in the body by `exp`)
   *
   * The following code:
   *
   * {{{
   * exp match {
   *   case C1(a, _) => b1
   *   case C2(a) => b2
   *   case C3() => b3
   * }
   * }}}
   *
   * is compiled to:
   *
   * {{{
   *   (match exp (
   *     ((C1 a _) b1)
   *     ((C2 a) b2)
   *     (C3 b3)))
   * }}}
   */
  def makePatternMatch(exp: Z3Exp, cases: List[Plugin.Case[Z3Exp]]): State[CompilerState, Z3Exp] = for {
    compiledCases <- cases.traverse(compileCase(exp, _))
  } yield Match(exp, compiledCases)

  def makeImport(pkg: String): State[CompilerState, Z3Exp] = State.pure("")

  def startFile(path: Option[String]): State[CompilerState, Z3Exp] = State.pure("")
  def endFile(): State[CompilerState, Z3Exp] = State.pure("")

  def makeLogicVar(name: String, tpe: Z3Exp): State[CompilerState, Z3Exp] = State.pure(Var(name, tpe))

  override def makeImplication(antecedent: Z3Exp, consequent: Z3Exp): State[CompilerState, Z3Exp] = State.pure {
    Apply("=>", List(antecedent, consequent))
  }

  override def makeForall(vars: Set[Z3Exp], body: Z3Exp): State[CompilerState, Z3Exp] = State.pure {
    Quantifier("forall", vars.toList, body)
  }

  override def makeExists(vars: Set[Z3Exp], body: Z3Exp): State[CompilerState, Z3Exp] = State.pure {
    Quantifier("exists", vars.toList, body)
  }

  def run(compiler: State[CompilerState, List[Z3Exp]], initialState: Option[CompilerState]): (CompilerState, String) = {
    // We add the built-in classes to the compiler's state such that we know they exist
    // this is important, for instance when checking if a type needs to be concretized, the type arguments must be concrete types (e.g. `Set[Int]`)
    // but if `Set` is not in the class table, then it won't be concretized
    val builtInClasses = Map(
      "Map" -> ClassInfo("Map", Map(), ""),
      "Set" -> ClassInfo("Set", Map(), ""),
      "Tuple" -> ClassInfo("Tuple", Map(), ""))
    val initState = initialState.getOrElse(CompilerState(builtInClasses))
    val state = compiler.run(initState).value._1
    val program = state.reversedProgram.reverse.mkString("\n\n")

    val compiledCode =
      s"""; Built-in classes
         |(declare-datatypes (T) ((Option None (Some (get T)))))
         |(define-sort Map (K V) (Array K (Option V)))
         |
         |; User-defined classes
         |$program""".stripMargin

    (state, compiledCode)
  }

  private def makeFunction(name: String, args: List[Plugin.Arg[Z3Exp]], body: Z3Exp, returnType: Z3Exp, recursive: Boolean): String = {
    val defStr = if (!recursive) "define-fun" else "define-fun-rec"
    val argsStr = args.map(arg => s"(${arg.name} ${arg.tpe})").mkString(" ")

    ind"""($defStr $name ($argsStr) $returnType
         |  $body)""".stripMargin
  }
}
