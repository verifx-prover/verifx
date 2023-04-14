package org.verifx.Compiler

/**
 * Intermediate representation of the code.
 * There are 5 broad categories of code: `Type`, `Term`, `Defn`, `Decl`, and `Logic`.
 */
object IR {
  sealed trait IR

  object Type {
    sealed trait Type extends IR
    sealed trait Primitive extends Type
    final case class Complex(name: String, targs: List[Type]) extends Type
    final case class Function(params: List[Type], res: Type) extends Type

    final case class TypeParameter(name: String, tparams: List[TypeParameter], upperTypeBound: Option[Type]) extends Type // e.g. `A` in `Foo[A[_, _]]`

    case object Boolean extends Primitive
    case object Int extends Primitive
    case object String extends Primitive
  }

  object Op {
    sealed trait Operator

    sealed trait BinaryOperator extends Operator
    case object Plus           extends BinaryOperator
    case object Minus          extends BinaryOperator
    case object Times          extends BinaryOperator
    case object Divide         extends BinaryOperator
    case object And            extends BinaryOperator
    case object Or             extends BinaryOperator
    case object Equals         extends BinaryOperator
    case object NotEquals      extends BinaryOperator
    case object SmallerThan    extends BinaryOperator
    case object SmallerOrEqual extends BinaryOperator
    case object BiggerThan     extends BinaryOperator
    case object BiggerOrEqual  extends BinaryOperator

    sealed trait UnaryOperator extends Operator
    case object Not extends UnaryOperator
    case object Negative extends UnaryOperator
  }

  sealed trait TermOrValDef extends IR

  object Term {
    sealed trait Term extends TermOrValDef
    final case class Block(terms: List[TermOrValDef]) extends Term

    sealed trait Literal extends Term
    final case class BoolL(value: Boolean)  extends Literal
    final case class IntL(value: Int)       extends Literal
    final case class StringL(value: String) extends Literal

    final case class Reference(name: String) extends Term // name that refers to a constant

    final case class BinaryOperation(op: Op.BinaryOperator, left: Term, right: Term) extends Term
    final case class UnaryOperation(op: Op.UnaryOperator, arg: Term) extends Term

    final case class InstantiateClass(clazz: String, tparams: List[Type.Type], args: List[Term]) extends Term
    final case class FieldAccess(clazz: Type.Type, obj: Term, field: String, tpe: Type.Type) extends Term
    final case class MethodCall(clazz: Type.Type, obj: Term, method: String, targs: List[Type.Type], args: List[Term], argTypes: List[Type.Type]) extends Term
    final case class MethodReference(clazz: Type.Type, obj: Term, method: String, signature: Type.Type) extends Term // reference to a method, e.g. `this.plus`

    final case class If(cond: Term, thenb: Term, elseb: Term) extends Term

    final case class Function(params: List[Defn.Arg], body: Term) extends Term
    final case class FunctionCall(fun: Term, args: List[Term]) extends Term

    final case class Case(pat: Pat.Pat, body: Term) extends IR
    final case class Match(exp: Term, cases: List[Case]) extends Term
  }

  object Pat {
    sealed trait Pat extends IR
    sealed trait VarOrWildcard extends Pat

    final case class Typed(pat: VarOrWildcard, tpe: Type.Type) extends Pat
    final case class Var(name: String) extends VarOrWildcard
    case object Wildcard extends VarOrWildcard
    final case class Extract(tpe: Type.Type, vars: List[VarOrWildcard]) extends Pat
  }

  object Mod {
    sealed trait Mod extends IR
    final case class Annot(name: String) extends Mod
    case object Sealed extends Mod
    case object Private extends Mod
    case object Protected extends Mod
    case object Override extends Mod
  }

  sealed trait Stat extends IR
  final case class Import(pkg: String) extends Stat

  object Decl {
    sealed trait Decl extends Stat
    final case class Val(name: String, tpe: Type.Type) extends Decl
    final case class Method(name: String, mods: List[Mod.Mod], tparams: List[Type.TypeParameter], args: List[Defn.Arg], retType: Type.Type) extends Decl
  }

  object Defn {
    sealed trait Defn extends IR

    final case class Arg(name: String, tpe: Type.Type) extends Defn // default value is not needed because omitted arguments are added by the compiler when doing the method call (or instantiating the class)
    final case class ValDef(name: String, tpe: Type.Type, value: Term.Term) extends Defn with TermOrValDef with Stat

    sealed trait MethodOrPredicate extends Defn with Stat
    sealed trait Predicate extends MethodOrPredicate

    final case class MethodDef(name: String, mods: List[Mod.Mod], tparams: List[Type.TypeParameter], args: List[Arg], body: Term.Term, retType: Type.Type, isRecursive: Boolean) extends MethodOrPredicate
    final case class CtxDef(method: String, tparams: List[Type.TypeParameter], args: List[Arg], body: Term.Term) extends Predicate
    final case class PreDef(method: String, tparams: List[Type.TypeParameter], args: List[Arg], body: Term.Term) extends Predicate
    final case class InvDef(method: String, tparams: List[Type.TypeParameter], args: List[Arg], body: Term.Term) extends Predicate

    final case class Field(mods: List[Mod.Mod], name: String, tpe: Type.Type) extends Defn
    final case class ClassDef(annots: List[Mod.Annot], name: String, tparams: List[Type.TypeParameter], fields: List[Field], methods: List[MethodOrPredicate], inheritedMethods: List[MethodOrPredicate], supert: Option[Type.Complex]) extends Defn
    // stats can be value definitions/declaration, method definitions/declaration, and method predicated
    final case class Trait(mods: List[Mod.Mod], name: String, tparams: List[Type.TypeParameter], stats: List[Stat], supert: Option[Type.Complex]) extends Defn

    final case class DataCtor(name: String, tparams: List[Type.TypeParameter], fields: List[Field]) extends Defn
    final case class Enum(name: String, tparams: List[Type.TypeParameter], ctors: List[DataCtor]) extends Defn
  }

  object Logic {
    sealed trait Logic extends IR

    sealed trait QuantifiedFormula extends Logic with Term.Term {
      val variables: Set[Variable]
      val body: Term.Term
    }

    final case class Variable(name: String, tpe: Type.Type) extends Logic
    final case class Implication(antecedent: Term.Term, consequent: Term.Term) extends Logic with Term.Term
    final case class Forall(variables: Set[Variable], body: Term.Term) extends QuantifiedFormula
    final case class Exists(variables: Set[Variable], body: Term.Term) extends QuantifiedFormula
  }
}
