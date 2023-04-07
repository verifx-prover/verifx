/**
 * = VeriFx: An Automated Verification Language =
 *
 * Welcome to VeriFx's documentation.
 * VeriFx is a functional OOP language, inspired by Scala, that supports fully automated verification of high-level correctness properties expressed in VeriFx itself.
 * This document describes the API of VeriFx's functional collections and includes a brief tutorial.
 *
 * === Object-Oriented Programming in VeriFx ===
 *
 * VeriFx supports classes, traits, and enumerations.
 * The syntax for class definitions is equivalent to Scala:
 *
 * {{{
 *   class MyClass[A](field1: Type1, field2: Type2) {
 *     def someMethod(): ReturnType = {
 *       val someNumber: Int = 5
 *       ...
 *     }
 *   }
 * }}}
 *
 * This defines a class with one type parameter `A`, two fields, and one method.
 * Note that the fields are immutable.
 * If some method needs to "change" one of the fields, you need to return a modified copy of the class.
 *
 * VeriFx has a type inferencer so you don't need to explicitly define the return type of methods.
 * Within a class, `this` refers to the object itself.
 * Hence, inside the body of the method you can access the fields as follows `this.field1`.
 * In contrast to Scala, the use of `this` is obligatory (as in Java), i.e. if you refer to `field1` directly the compiler will complain.
 *
 * Sometimes, you may want to write recursive methods.
 * These need to be annotated with the `@recursive` annotation and always require a return type, e.g.:
 *
 * {{{
 *   @recursive
 *   def fib(n: Int): Int = {
 *     if (n == 0 || n == 1)
 *       n
 *     else
 *       this.fib(n-1) + this.fib(n-2)
 *   }
 * }}}
 *
 * Classes are instantiated using the `new` keyword, e.g. `new Foo(1, 2)`.
 *
 * Sometimes you want to share some code across several classes to avoid code duplication.
 * To this end, VeriFx supports traits.
 * Classes and traits can extend from traits (single-inheritance only!).
 *
 * {{{
 *   trait Adder {
 *     def plus(x: Int, y: Int) = x+y
 *   }
 *
 *   class Arithmetic extends Adder {
 *     def plusOne(x: Int) = this.plus(x, 1)
 *   }
 * }}}
 *
 * Traits can be polymorphic and a trait's type parameters can have upper type bounds:
 *
 * {{{
 *   trait CRDTProof[T <: CRDT[T]] {
 *     ...
 *   }
 * }}}
 *
 * Finally, VeriFx also supports enumerations (aka algebraic data types):
 *
 * {{{
 *   object Cmd {
 *     enum Cmd {
 *       Inc(x: Int) | Dec(x: Int) | Nop()
 *     }
 *   }
 * }}}
 *
 * Enumerations are defined using the `enum` keyword and should always be nested within an object.
 * The `Cmd` enumeration defines 3 constructors `Inc`, `Dec`, and `Nop`.
 * These constructors can define fields.
 *
 * Enumerations can be instantiated through one of their constructors.
 * The resulting object is of type `Cmd` and can be deconstructed by pattern matching on it.
 * For example:
 *
 * {{{
 *   def apply(number: Int, cmd: Cmd) = cmd match {
 *     case Inc(a) => number + a
 *     case Dec(a) => number - a
 *     case Nop() => number
 *   }
 * }}}
 *
 * === Built-in Collections ===
 *
 * VeriFx features a number of built-in functional collections: `LList`, `Map`, `Set`, `Tuple`, `Vector`.
 * The complete API of these collections can be found in this documentation.
 * Use the search bar to look up a data type or method.
 *
 * === Verifying Correctness Properties ===
 *
 * VeriFx programs provide a special proof construct.
 * A proof describes a correctness property about some VeriFx program and will be verified automatically.
 * Proofs are defined using the `proof` keyword and expect a name and a body.
 * The body must be a boolean expression such that its satisfiability can be checked.
 * Proofs cannot be top-level and should always be defined inside an object.
 *
 * {{{
 *   object MyProof {
 *     proof plusCommutes {
 *       forall (x: Int, y: Int) {
 *         x+y == y+x
 *       }
 *     }
 *   }
 * }}}
 *
 * Proofs can also be polymorphic:
 *
 * {{{
 *   proof setConverges[T] {
 *     forall (x: GSet[T], y: GSet[T]) {
 *       x.merge(y) == y.merge(x)
 *     }
 *   }
 * }}}
 *
 * Besides the regular programming constructs,
 * proofs can also contain logical constructs such as quantified formulas and logical implication:
 *
 *  - `forall (var1: Type1, ..., varN: TypeN) { booleanExp }`
 *  - `exists (var1: Type1, ..., varN: TypeN) { booleanExp }`
 *  - `booleanExp =>: booleanExp`
 *
 * === Organizing Code in Several Files ===
 *
 * In order to keep your project comprehensible, source code can be spread over several files.
 * Files can import other files using the `import` keyword but this should always occur at the top of the file before anything else:
 *
 * {{{
 *   import be.vub.crdtproofs.GCounter
 * }}}
 *
 * Note that imports are different than in Scala.
 * The above import statement will import everything from the `GCounter` file
 * that should be located at `src/be/vub/crdtproofs/GCounter.vfx`.
 * Thus, the import statement merely encodes the path to the file, starting from the `src` folder.
 *
 **/
package object verifx
