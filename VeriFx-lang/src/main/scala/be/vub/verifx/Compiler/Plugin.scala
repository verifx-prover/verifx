package be.vub.verifx.Compiler

import cats.data.State

object Plugin {
  final case class Arg[C](name: String, tpe: C)
  final case class Field[C](mods: List[C], name: String, tpe: C)
  final case class EnumCtor[C](name: String, tparams: List[C], fields: List[Field[C]])

  object Pat {
    sealed trait Pat
    sealed trait VarOrWildcard extends Pat

    final case class Typed[C](pat: VarOrWildcard, tpe: C) extends Pat

    final case class Var(name: String) extends VarOrWildcard
    case object Wildcard extends VarOrWildcard
    final case class Extract[C](tpe: C, vars: List[VarOrWildcard]) extends Pat
  }

  final case class Case[C](pat: Pat.Pat, body: C)
}

/**
 * Interface that must be implemented by compiler plugins to compile VeriFx code into their language.
 */
trait Plugin[C, M <: C, S] {
  import IR._
  import Plugin._

  val int_type: C
  val bool_type: C
  val string_type: C

  /**
   * Called when starting to compile a new file
   * @param path Path to the file. May not be defined when compiling source code using `ProgramCompiler`.
   */
  def startFile(path: Option[String]): State[S, C]

  /**
   * Called when a file is completely compiled
   */
  def endFile(): State[S, C]

  def makeComplexType(name: String, compiledTypeArgs: List[C]): State[S, C]
  def makeTypeParam(name: String, tparams: List[C], upperTypeBound: Option[C]): State[S, C]
  def makeFunctionType(paramTypes: List[C], resType: C): State[S, C]

  def makeLiteral(lit: Term.Literal): State[S, C]

  def makeBinaryOperation(op: Op.BinaryOperator, left: C, right: C): State[S, C]
  def makeUnaryOperation(op: Op.UnaryOperator, arg: C): State[S, C]

  def makeReference(ref: String): State[S, C]
  def makeIf(condition: C, thenb: C, elseb: C): State[S, C]
  def makeBlock(block: List[C]): State[S, C]

  def instantiateClass(className: String, tparams: List[C], args: List[C]): State[S, C]
  def makeFieldAccess(classType: C, obj: C, field: String, tpe: C): State[S, C]
  def makeMethodReference(classType: C, obj: C, method: String, signature: C): State[S, C]
  def makeMethodCall(classType: C, obj: C, method: String, targs: List[C], args: List[C], argTypes: List[C]): State[S, C]

  def makeValDef(name: String, tpe: C, value: C): State[S, C]
  def makeValDecl(name: String, tpe: C): State[S, C]

  def makeFunction(params: List[Arg[C]], body: C): State[S, C]
  def makeFunctionCall(fun: C, args: List[C]): State[S, C]

  def makePatternMatch(exp: C, cases: List[Plugin.Case[C]]): State[S, C]

  def makeMethodDef(name: String, mods: List[C], tparams: List[C], args: List[Arg[C]], body: C, retTpe: C, isRecursive: Boolean): State[S, M]
  def makeMethodDecl(mods: List[C], name: String, tparams: List[C], args: List[Arg[C]], retTpe: C): State[S, C]
  def makeContextDef(name: String, tparams: List[C], args: List[Arg[C]], body: C): State[S, M]
  def makePreconditionDef(name: String, tparams: List[C], args: List[Arg[C]], body: C): State[S, M]
  def makeInvariantDef(name: String, tparams: List[C], args: List[Arg[C]], body: C): State[S, M]

  def makeClassDef(annots: List[String], name: String, tparams: List[C], fields: List[Field[C]], methodsAndPreds: List[M], inheritedMethodsAndPreds: List[M], supert: Option[C]): State[S, C]
  def makeTraitDef(mods: List[C], name: String, tparams: List[C], stats: List[C], supert: Option[C]): State[S, C]
  def makeEnumDef(name: String, tparams: List[C], ctors: List[EnumCtor[C]]): State[S, C]

  def makeMod(mod: IR.Mod.Mod): State[S, C]
  def makeImport(pkg: String): State[S, C]

  def makeLogicVar(name: String, tpe: C): State[S, C]
  def makeImplication(antecedent: C, consequent: C): State[S, C] = for {
    ante <- makeUnaryOperation(Op.Not, antecedent)
    boolFormula <- makeBinaryOperation(Op.Or, ante, consequent) // a =>: b translates to !a || b
  } yield boolFormula
  // Quantified formulas and implication can only be translated to Z3
  // so by default, they are assumed to be true
  def makeForall(vars: Set[C], body: C): State[S, C] = makeLiteral(IR.Term.BoolL(true))
  def makeExists(vars: Set[C], body: C): State[S, C] = makeLiteral(IR.Term.BoolL(true))

  /**
   * The compiler is still waiting for the internal state in order to kick off the compilation.
   * This method must run the compiler with the initial compiler state and return the compiled code.
   * The value contained by the state monad is the list of all compiled statements.
   */
  def run(compiler: State[S, List[C]], initialState: Option[S]): (S, String)
}

