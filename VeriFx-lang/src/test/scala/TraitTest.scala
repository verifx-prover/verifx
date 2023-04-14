import org.verifx.verifx.Compiler.Plugins.{ScalaCompilerPlugin, Z3CompilerPlugin}
import org.verifx.verifx.Compiler.ProgramCompiler
import org.verifx.verifx.{ParseError, TypeError}
import org.scalatest.FlatSpec

class TraitTest extends FlatSpec {

  // Test traits:
  // - you can pass subtypes              DEPRECATED --> arguments must be concrete!
  // - declare fields in traits           OK
  // - implement declared fields          OK
  // - get error if not implementing declared fields OK
  // - reject concrete field in traits    OK
  // - declare methods in trait           OK
  // - implement declared methods         OK
  // - provide concrete method            OK
  // - override concrete method           OK
  // - override field/method with wrong type should give error                  OK
  // - provide a concrete field/method that depends on a declared field/method  OK
  // - allow upper type bounds on trait parameters        OK
  // - allow type parameters to introduce types that are only valid in the upper type bound OK
  // - do not allow lower type bounds on trait parameters OK

  val fooTrait =
    s"""trait Foo {
          val a: Int
        }
          """.stripMargin

  "Classes" should "support type parameters" in {
    val classWithTParam =
      """class Test[A](a: A)
        |
        |class Bar {
        |  def t() = new Test(5)
        |}""".stripMargin

    assert(ProgramCompiler(classWithTParam).compileToLanguage(new ScalaCompilerPlugin)._2 != "")

    val classWithWrongExplicitTArg =
      """class Test[A](a: A)
        |
        |class Bar {
        |  def t() = new Test[String](5)
        |}""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(classWithWrongExplicitTArg).compileToLanguage(new ScalaCompilerPlugin)
    }
  }

  it should "reject subtypes" in {
    val subTypeInCtor =
      s"""trait Foo
          class B(f: Foo)
          """.stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(subTypeInCtor).compileToLanguage(new ScalaCompilerPlugin)
    }

    val subTypeInMethod =
      s"""trait Foo[V]
          class A {
            def test(f: Foo[A]) = f
          }
          """.stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(subTypeInMethod).compileToLanguage(new ScalaCompilerPlugin)
    }

    val subTypeInRetType =
      s"""trait Foo[V]
          class B extends Foo[Int]
          class A {
            def test(b: B): Foo[Int] = b
          }
          """.stripMargin

    val subTypeInRetTypeTest = ProgramCompiler(subTypeInRetType).compileToLanguage(new ScalaCompilerPlugin)._2
    assert(subTypeInRetTypeTest != "")

    val sealedTraitInMethod =
      s"""sealed trait Foo
          class A extends Foo
          class B extends Foo
          class C {
            def test(f: Foo) = f
          }""".stripMargin

    val sealedTraitInMethodTest = ProgramCompiler(sealedTraitInMethod).compileToLanguage(new ScalaCompilerPlugin)._2
    assert(sealedTraitInMethodTest != "")
  }

  "Traits" should "support value declarations" in {
    // Check that value declarations are supported
    val declarationOnlyTest = ProgramCompiler(fooTrait).compileToLanguage(new ScalaCompilerPlugin)._2
    assert(declarationOnlyTest != "")

    // Check that traits can declare the same value as a super trait
    val declarationTwice =
      s"""$fooTrait
          trait Bar extends Foo {
            val a: Int
          }
          """.stripMargin

    val declarationTwiceTest = ProgramCompiler(declarationTwice).compileToLanguage(new ScalaCompilerPlugin)._2
    assert(declarationTwiceTest != "")

    // Check that traits cannot declare the same value with different types
    val declarationTwiceWithDifferentTypes =
      s"""$fooTrait
          trait Bar extends Foo {
            val a: String
          }
          """.stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(declarationTwiceWithDifferentTypes).compileToLanguage(new ScalaCompilerPlugin)
    }

    // Check that the sub trait can declare a value that is more precise than the super trait's declaration
    val declareSubtype =
      s"""trait A
          trait B extends A

          trait C {
            val a: A
          }

          trait D extends C {
            val a: B
          }

          class E() extends B
          class F(a: E) extends D
          """.stripMargin

    assert(ProgramCompiler(declareSubtype).compileToLanguage(new ScalaCompilerPlugin)._2 != "")
  }

  it should "reject value definitions" in {
    val valDef =
      """trait Foo {
           val a: Int = 5
         }""".stripMargin

    assertThrows[ParseError] {
      assert(ProgramCompiler(valDef).compileToLanguage(new ScalaCompilerPlugin)._2 != "")
    }
  }

  it should "throw an error if field is declared twice" in {
    val twice =
      """trait Foo {
           val a: Int
           val a: Int
         }
         """.stripMargin

    assertThrows[ParseError] {
      ProgramCompiler(twice).compileToLanguage(new ScalaCompilerPlugin)
    }
  }

  it should "support method declarations and definitions" in {
    val declareMethod =
      """trait Foo {
           def plus(a: Int, b: Int): Int
         }""".stripMargin

    assert(ProgramCompiler(declareMethod).compileToLanguage(new ScalaCompilerPlugin)._2 != "")

    val defineMethod =
      """trait Foo {
           def plus(a: Int, b: Int): Int = a+b
         }""".stripMargin

    assert(ProgramCompiler(defineMethod).compileToLanguage(new ScalaCompilerPlugin)._2 != "")
  }

  it should "be able to override methods" in {
    val overrideMethod =
      """trait Foo {
           def plus(a: Int, b: Int): Int = a+b
         }
         trait Bar extends Foo {
           override def plus(a: Int, b: Int): Int = a+a+b
         }
         """.stripMargin

    assert(ProgramCompiler(overrideMethod).compileToLanguage(new ScalaCompilerPlugin)._2 != "")

    val overrideMethodWithoutMod =
      """trait Foo {
           def plus(a: Int, b: Int): Int = a+b
         }
         trait Bar extends Foo {
           def plus(a: Int, b: Int): Int = a+a+b
         }
         """.stripMargin

    assertThrows[ParseError] {
      ProgramCompiler(overrideMethodWithoutMod).compileToLanguage(new ScalaCompilerPlugin)
    }

    val overrideNothing =
      """trait Foo {
           def get(a: Int): Int = a
         }
         trait Bar extends Foo {
           override def plus(a: Int): Int = a + 1
         }
         """.stripMargin

    assertThrows[ParseError] {
      ProgramCompiler(overrideNothing).compileToLanguage(new ScalaCompilerPlugin)
    }

    val overrideMethodWithWrongSignature =
      """trait Foo {
           def plus(a: Int): Int = a + 1
         }
         trait Bar extends Foo {
           override def plus(a: String): Int = 5
         }
         """.stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(overrideMethodWithWrongSignature).compileToLanguage(new ScalaCompilerPlugin)
    }
  }

  it should "be able to use declared fields" in {
    val useDeclaredField =
      """trait Foo {
           val a: Int
           val b: Int
           def plus() = this.a + this.b
         }
         """.stripMargin

    assert(ProgramCompiler(useDeclaredField).compileToLanguage(new ScalaCompilerPlugin)._2 != "")

    val useSuperField =
      """trait Foo {
           val a: Int
           val b: Int
         }
         trait Bar extends Foo {
           def plus() = this.a + this.b
         }
         """.stripMargin

    assert(ProgramCompiler(useSuperField).compileToLanguage(new ScalaCompilerPlugin)._2 != "")
  }

  /*
      - Check that if we provide a predicate on an inexistant method that it throws an error
      - Check that override modifiers are required on method predicates
      - Check that we override a method predicate if we provide override
   */

  it should "be able to override method predicates" in {
    val traitWithPred =
      """trait Foo {
           pre divide(a: Int, b: Int) {
             b != 0
           }

           def divide(a: Int, b: Int) = a/b
         }

         class Bar extends Foo {
           pre divide(a: Int, b: Int) {
             a > 0 && b > 0
           }
         }
         """.stripMargin

    assert(ProgramCompiler(traitWithPred).compileToLanguage(new ScalaCompilerPlugin)._2 != "")
  }

  it should "support type constructors in type parameter list" in {
    val traitWithTpeCtor =
      """trait Foo[S[_]] {
        |  val set: S[Int]
        |}
        |class Bar(set: Set[Int]) extends Foo[Set]""".stripMargin

    assert(ProgramCompiler(traitWithTpeCtor).compileToLanguage(new ScalaCompilerPlugin)._2 != "")

    val traitWithTpeCtorWrong =
      """trait Foo[S[_]] {
           val set: S[Int]
         }
         class Bar(set: Set[Int]) extends Foo[Set[Int]]
         """.stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(traitWithTpeCtorWrong).compileToLanguage(new ScalaCompilerPlugin)
    }
  }

  it should "throw an error if a predicate is associated to an inexistant method" in {
    val invalidPred =
      """trait Test {
           inv foo(a: Int) {
             true
           }
         }
         """.stripMargin

    assertThrows[ParseError] {
      ProgramCompiler(invalidPred).compileToLanguage(new ScalaCompilerPlugin)
    }
  }

  it should "require value declarations to have concrete types" in {
    val correctValueDecl =
      """trait Test {
           val foo: Int
         }""".stripMargin

    assert(ProgramCompiler(correctValueDecl).compileToLanguage(new ScalaCompilerPlugin)._2 != "")

    val wrongValueDecl =
      """trait Test {
           val foo: Unknown
         }""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(wrongValueDecl).compileToLanguage(new ScalaCompilerPlugin)
    }

    val wrongValueDecl2 =
      """trait Test[AB[_]] {
           val foo: AB
         }""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(wrongValueDecl2).compileToLanguage(new ScalaCompilerPlugin)
    }
  }

  it should "require methods to have concrete argument and return types" in {
    val correctMethodDeclAndDef =
      """trait Foo[A[_], B] {
           def test(a: A[Int], b: B): Int
           def bar(a: A[Int], b: B): Int = 5
         }""".stripMargin

    assert(ProgramCompiler(correctMethodDeclAndDef).compileToLanguage(new ScalaCompilerPlugin)._2 != "")

    val wrongMethodDeclArg =
      """trait Foo[A[_], B] {
           def test(a: A, b: B): Int
         }""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(wrongMethodDeclArg).compileToLanguage(new ScalaCompilerPlugin)
    }

    val wrongMethodDeclRetTpe =
      """trait Foo[A[_], B] {
           def test(a: A[Int], b: B): Set
         }""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(wrongMethodDeclRetTpe).compileToLanguage(new ScalaCompilerPlugin)
    }

    val wrongMethodDefArg =
      """trait Foo[A[_], B] {
           def test(a: A, b: B): Int = 5
         }""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(wrongMethodDefArg).compileToLanguage(new ScalaCompilerPlugin)
    }
  }

  it should "support type parameters with upper type bounds" in {
    val traitDef1 =
      """trait CvRDT {
        |  val a: Int
        |}
        |
        |class Foo(a: Int) extends CvRDT
        |
        |trait CRDTProof[V <: CvRDT] {
        |  def test(x: V) = x.a
        |}
        |
        |class Test extends CRDTProof[Foo]""".stripMargin


    val res1 = ProgramCompiler(traitDef1).compileToLanguage(new Z3CompilerPlugin)
    assert(res1._2 != "")

    val traitDef2 =
      """trait CvRDT {
        |  val a: Int
        |}
        |
        |class Foo(a: Int) extends CvRDT
        |
        |trait CRDTProof[V <: CvRDT] {
        |  def test(x: V) = x.a
        |}
        |
        |trait Intermediate[A <: CvRDT] extends CRDTProof[A]
        |
        |class Test extends Intermediate[Foo]""".stripMargin

    val res2 = ProgramCompiler(traitDef2).compileToLanguage(new Z3CompilerPlugin)
    assert(res2._2 != "")

    val traitDef3 =
      """trait CRDT[A]
        |class Foo extends CRDT[Foo]
        |
        |trait Test[T <: CRDT[T]]
        |class Bar extends Test[Foo]""".stripMargin

    val res3 = ProgramCompiler(traitDef3).compileToLanguage(new ScalaCompilerPlugin)._2
    assert(res3 != "")
  }

  it should "throw an error if type arguments are not subtypes of the type parameters' upper type bounds" in {
    val traitDef1 =
      """trait CvRDT {
        |  val a: Int
        |}
        |
        |class Foo(a: Int)
        |
        |trait CRDTProof[V <: CvRDT] {
        |  def test(x: V) = x.a
        |}
        |
        |class Test extends CRDTProof[Foo]""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(traitDef1).compileToLanguage(new Z3CompilerPlugin)
    }

    val traitDef2 =
      """trait CvRDT {
        |  val a: Int
        |}
        |
        |class Foo(a: Int) extends CvRDT
        |
        |trait CRDTProof[V <: CvRDT] {
        |  def test(x: V) = x.a
        |}
        |
        |trait Intermediate[A] extends CRDTProof[A]
        |
        |class Test extends Intermediate[Foo]""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(traitDef2).compileToLanguage(new Z3CompilerPlugin)
    }

    val traitDef3 =
      """trait CvRDT {
        |  val a: Int
        |}
        |
        |class Foo(a: Int) extends CvRDT
        |
        |trait CRDTProof[A <: CvRDT, B <: A] {
        |  def test(x: B) = x.a
        |}
        |
        |class Test extends CRDTProof[Foo, Foo]""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(traitDef3).compileToLanguage(new Z3CompilerPlugin)
    }

    val traitDef4 =
      """trait CRDT[A]
        |class Foo extends CRDT[Int]
        |
        |trait Test[T <: CRDT[T]]
        |class Bar extends Test[Foo]""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(traitDef4).compileToLanguage(new Z3CompilerPlugin)
    }
  }

  it should "allow type parameters to introduce types that are valid in the upper type bound" in {
    val traitDef =
      """trait CRDT[A]
        |class ORSet[V] extends CRDT[ORSet[V]]
        |
        |trait CRDTProofGeneric[ T[S] <: CRDT[T[S]] ]
        |class Test() extends CRDTProofGeneric[ORSet]""".stripMargin

    val res = ProgramCompiler(traitDef).compileToLanguage(new ScalaCompilerPlugin)._2
    assert(res != "")
  }

  it should "throw an error if type argument does not match the type parameter's upper type bound" in {
    val traitDef1 =
      """trait CRDT[A]
        |class ORSet[V] extends CRDT[ORSet[Int]]
        |
        |trait CRDTProofGeneric[ T[S] <: CRDT[T[S]] ]
        |class Test() extends CRDTProofGeneric[ORSet]""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(traitDef1).compileToLanguage(new ScalaCompilerPlugin)._2
    }

    val traitDef2 =
      """trait CRDT[A]
        |class ORSet[V] extends CRDT[ORSet[V]]
        |
        |trait CRDTProofGeneric[ T[S] <: CRDT[T[S]] ]
        |class Test[V]() extends CRDTProofGeneric[ORSet[V]]""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(traitDef2).compileToLanguage(new ScalaCompilerPlugin)._2
    }
  }

  it should "allow type parameters to be used in upper type bound of other type parameters" in {
    val traitDef =
      """trait CmRDT[Op, T <: CmRDT[Op, T]]
        |class Counter(ctr: Int = 0) extends CmRDT[Int, Counter]""".stripMargin

    val res = ProgramCompiler(traitDef).compileToLanguage(new ScalaCompilerPlugin)._2
    assert(res != "")
  }

  it should "throw an error if type parameters have lower type bounds" in {
    val traitDef =
      """trait CvRDT {
        |  val a: Int
        |}
        |
        |trait CRDTProof[A >: CvRDT]""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(traitDef).compileToLanguage(new Z3CompilerPlugin)
    }
  }

  "Sealed traits" should "be supported" in {
    val sealedTrait =
      s"""sealed trait Foo""".stripMargin

    assert(ProgramCompiler(sealedTrait).compileToLanguage(new ScalaCompilerPlugin)._2 != "")
  }

  it should "throw an error if body is non empty" in {
    val sealedTraitWithBody =
      s"""sealed trait Foo {
            val a: Int
          }""".stripMargin

    assertThrows[ParseError] {
      ProgramCompiler(sealedTraitWithBody).compileToLanguage(new ScalaCompilerPlugin)
    }
  }

  "Classes" should "be able to implement declared values" in {
    // Check that we can implement a declared field
    val implementingDeclaration =
      s"""$fooTrait
          class Bar(a: Int) extends Foo
          """.stripMargin

    val implementingDeclarationTest = ProgramCompiler(implementingDeclaration).compileToLanguage(new ScalaCompilerPlugin)._2
    assert(implementingDeclarationTest != "")

    val implementingDeclarationGeneric =
      """trait Foo[V] {
           val a: V
         }
         class Bar(a: Int) extends Foo[Int]
         """.stripMargin

    val implementingDeclarationGenericTest = ProgramCompiler(implementingDeclarationGeneric).compileToLanguage(new ScalaCompilerPlugin)._2
    assert(implementingDeclarationGenericTest != "")
  }

  it should "be forced to implement declared fields" in {
    // Check that we get an error if we do not implement a declared value
    val notImplementingDeclaration =
      """trait Foo[V] {
           val a: V
         }
         trait Bar extends Foo[Int]
         class Baz() extends Bar
         """.stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(notImplementingDeclaration).compileToLanguage(new ScalaCompilerPlugin)
    }
  }

  it should "be able to implement declared methods" in {
    val implementDeclared =
      """trait Foo {
           def plus(a: Int, b: Int): Int
         }
         class Bar extends Foo {
           def plus(a: Int, b: Int): Int = a+b
         }
         """.stripMargin

    assert(ProgramCompiler(implementDeclared).compileToLanguage(new ScalaCompilerPlugin)._2 != "")

    val overrideMethod =
      """trait Foo {
           def plus(a: Int, b: Int): Int = a+b
         }
         class Bar extends Foo {
           override def plus(a: Int, b: Int): Int = a+a+b
         }
         """.stripMargin

    assert(ProgramCompiler(overrideMethod).compileToLanguage(new ScalaCompilerPlugin)._2 != "")

    val implementWithoutOverride =
      """trait Foo {
           def plus(a: Int, b: Int): Int = a+b
         }
         class Bar extends Foo {
           def plus(a: Int, b: Int): Int = a+a+b
         }
         """.stripMargin

    assertThrows[ParseError] {
      ProgramCompiler(implementWithoutOverride).compileToLanguage(new ScalaCompilerPlugin)
    }

    val implementWrongSignature =
      """trait A
         class B() extends A
         trait Foo {
           def plus(a: A): A
         }
         class Bar extends Foo {
           def plus(b: B): B = new B()
         }
         """.stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(implementWrongSignature).compileToLanguage(new ScalaCompilerPlugin)
    }

    val implementMoreSpecificSignature =
      """trait A
         class B() extends A
         trait Foo {
           def plus(a: Int): A
         }
         class Bar extends Foo {
           def plus(a: Int): B = new B()
         }
         """.stripMargin

    assert(ProgramCompiler(implementMoreSpecificSignature).compileToLanguage(new ScalaCompilerPlugin)._2 != "")
  }

  it should "be forced to implement declared methods" in {
    val notImplementing =
      """trait Foo {
           def plus(a: Int, b: Int): Int
         }
         class Bar extends Foo
         """.stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(notImplementing).compileToLanguage(new ScalaCompilerPlugin)
    }
  }

  it should "support type parameters to be type constructors" in {
    val classWithTpeCtor = "class Foo[ABCD[_]](a: ABCD[Int])"

    assert(ProgramCompiler(classWithTpeCtor).compileToLanguage(new ScalaCompilerPlugin)._2 != "")

    // Now try an invalid class definition where the type of a field is not concrete
    // i.e. it's type is a type constructor
    val classWithWrongTpeCtorUse = "class Foo[ABCD[_]](a: ABCD)"

    assertThrows[TypeError] {
      ProgramCompiler(classWithWrongTpeCtorUse).compileToLanguage(new ScalaCompilerPlugin)
    }
  }

  it should "require methods to have concrete argument and return types" in {
    val correctMethodDef =
      """class Foo[A[_], B] {
           def bar(a: A[Int], b: B): Int = 5
         }""".stripMargin

    assert(ProgramCompiler(correctMethodDef).compileToLanguage(new ScalaCompilerPlugin)._2 != "")

    val wrongMethodDefArg =
      """class Foo[A[_], B] {
           def test(a: A, b: B): Int = 5
         }""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(wrongMethodDefArg).compileToLanguage(new ScalaCompilerPlugin)
    }

    val wrongValDef =
      """class Foo[A[_], B] {
           def test(a: A[Int], b: B): Int = {
             val test: Set = new Set[Int]()
             5
           }
         }""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(wrongValDef).compileToLanguage(new ScalaCompilerPlugin)
    }

    val nonConcreteRetTpe =
      """trait Bar
         class Baz extends Bar
         class Foo[A[_], B] {
           def test(a: A[Int], b: B): Bar = new Baz().asInstanceOf[Bar]
         }""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(nonConcreteRetTpe).compileToLanguage(new ScalaCompilerPlugin)
    }
  }


}
