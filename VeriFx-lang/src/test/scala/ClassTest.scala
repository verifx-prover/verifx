import org.verifx.verifx.Compiler.Plugins.{ScalaCompilerPlugin, Z3CompilerPlugin}
import org.verifx.verifx.Compiler.ProgramCompiler
import org.verifx.verifx.{ParseError, TypeError}
import org.scalatest.FlatSpec

class ClassTest extends FlatSpec {
  "Compiler" should "throw an error when operators are used on wrong values" in {
    val invalidOp =
      """class Test {
           def foo() = 1 + "a"
         }""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(invalidOp).compileToLanguage(new ScalaCompilerPlugin)
    }
  }

  it should "support objects" in {
    val obj =
      """object Test {
           def foo() = 2
           enum A {
             B() | C()
           }
           enum D {
             E() | F()
           }
           def bar() = 3
           proof foobar {
             this.foo() + this.bar() == 5
           }
           proof barfoo {
             this.bar() + this.foo() == 5
           }
         }""".stripMargin

    val scalaCode = ProgramCompiler(obj).compileToLanguage(new ScalaCompilerPlugin)._2
    val z3Code = ProgramCompiler(obj).compileToLanguage(new Z3CompilerPlugin)._2

    assert(scalaCode != "")
    assert(z3Code != "")
  }

  it should "support if statement" in {
    val standardIf =
      """class Test {
           def foo(a: Int) = {
             if (a == 1)
               true
             else
               false
           }
         }""".stripMargin

    assert(ProgramCompiler(standardIf).compileToLanguage(new ScalaCompilerPlugin)._2 != "")

    // The below example should throw an error because the if expression is of type Bar which is a trait
    // and trait types don't exist in Z3
    val ifSubtypesWrong =
      """trait Foo

         trait Bar extends Foo
         trait Baz extends Bar

         class C1 extends Bar
         class C2 extends Baz

         class Test {
           def foo(a: Int) = {
             if (a == 1)
               new C1()
             else
               new C2()

             true
           }
         }""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(ifSubtypesWrong).compileToLanguage(new ScalaCompilerPlugin)
    }

    // The below example works because the if expression is of type Op which is an enumeration and enumerations exist in Z3
    val ifSubtypesEnum =
      """object Test {
           enum Op[V] {
             Ins(pos: Int, c: V) | Del(pos: Int)
           }
         }

         class Test {
           def foo(a: Int) = {
             if (a == 1)
               new Ins(5, "abc")
             else
               new Del[String](6)
           }
         }""".stripMargin

    assert(ProgramCompiler(ifSubtypesEnum).compileToLanguage(new ScalaCompilerPlugin)._2 != "")

    // This should throw an error because the then branch has type Op[String]
    // while the then branch has type Op2[String]
    val ifSubtypesEnumWrong =
      """object Test {
           enum Op[V] {
             Ins(pos: Int, c: V) | Del(pos: Int)
           }

           enum Op2[V] {
             Ins2(pos: Int, c: V) | Del2(pos: Int)
           }
         }

         class Test {
           def foo(a: Int) = {
             if (a == 1)
               new Ins(5, "abc")
             else
               new Del2[String](6)
           }
         }""".stripMargin

    //assertThrows[TypeError] {
      assert(ProgramCompiler(ifSubtypesEnum).compileToLanguage(new ScalaCompilerPlugin)._2 != "")
   // }

    val ifNestedSubtypesEnum =
      """object Test {
           enum Op[V] {
             Ins(pos: Int, c: V) | Del(pos: Int)
           }
         }

         class Test {
           def foo(a: Int) = {
             if (a == 1)
               new Tuple(new Ins(5, "abc"), 5)
             else
               new Tuple(new Del[String](6), 5)
           }
         }""".stripMargin

    assert(ProgramCompiler(ifNestedSubtypesEnum).compileToLanguage(new ScalaCompilerPlugin)._2 != "")

    // The below example should throw an error because the true branch is of type Ins[String] and the else branch of type Del[Int] so there is no common super type
    val ifSubtypesEnumWrong2 =
      """object Test {
           enum Op[V] {
             Ins(pos: Int, c: V) | Del(pos: Int)
           }
         }

         class Test {
           def foo(a: Int) = {
             if (a == 1)
               new Ins(5, "abc")
             else
               new Del[Int](6)
           }
         }""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(ifSubtypesEnumWrong2).compileToLanguage(new ScalaCompilerPlugin)
    }

    val ifDifferentTypes =
      """class Test {
           def foo(a: Int) = {
             if (a == 1)
               5
             else
               true

             true
           }
         }""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(ifDifferentTypes).compileToLanguage(new ScalaCompilerPlugin)
    }
  }

  "Classes" should "throw an error if a predicate is associated to an inexistant method" in {
    val invalidPred =
      """class Test {
           inv foo(a: Int) {
             true
           }
         }
         """.stripMargin

    assertThrows[ParseError] {
      ProgramCompiler(invalidPred).compileToLanguage(new ScalaCompilerPlugin)
    }
  }

  it should "throw an error if wrong arguments are passed to class constructor" in {
    val invalidInstantiation =
      """class Test(a: Int)
         class Foo {
           def foo() = new Test(true)
         }
         """.stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(invalidInstantiation).compileToLanguage(new ScalaCompilerPlugin)
    }

    val invalidInstantiationWithTypeArgs =
      """class Test[A](a: A)
         class Foo {
           def foo() = new Test[Int](true)
         }
         """.stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(invalidInstantiationWithTypeArgs).compileToLanguage(new ScalaCompilerPlugin)
    }
  }

  it should "support type parameters" in {
    val classWithTypeParams =
      """class Test[A, B[_]] {
           def foo(b: Vector[B[A]]) = {
             val id = (x: Vector[B[A]]) => x
             id(b)
           }
         }""".stripMargin

    assert(ProgramCompiler(classWithTypeParams).compileToLanguage(new Z3CompilerPlugin)._2 != "")
  }

  it should "support default arguments in methods" in {
    val defaultArgs =
      """class Test(a: Int, b: Int) {
           def copy(a: Int = this.a, b: Int = this.b) = new Test(a, b)
         }

         class Bar() {
           def test() = {
             val t = new Test(2, 3)
             t.copy()
           }
         }
         """.stripMargin

    assert(ProgramCompiler(defaultArgs).compileToLanguage(new ScalaCompilerPlugin)._2 != "")
  }

  it should "support built-in sets and maps" in {
    val test =
      """class Test(t: Set[String] = new Set[String]()) {
           def foo(a: Set[Int] = new Set[Int]()) = a
           def bar(a: Map[Int, String] = new Map[Int, String]()) = a
         }

         class Bar {
           def test() = (new Test()).foo()
         }
         """.stripMargin

    assert(ProgramCompiler(test).compileToLanguage(new ScalaCompilerPlugin)._2 != "")
  }

  it should "support fields with a function type" in {
    val classWithFunctionField =
      """class Test[A, B, C](f: (A, B) => C)
         class Foo {
           def bar() = new Test((a: Int, b: String) => true)
           def baz() = this.bar().f(1, "test")
         }
         """.stripMargin

    assert(ProgramCompiler(classWithFunctionField).compileToLanguage(new ScalaCompilerPlugin)._2 != "")
  }

  it should "not allow functions with zero arguments" in {
    val functionZeroArgs =
      """class Foo {
           def bar() =
             () => true
         }
         """.stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(functionZeroArgs).compileToLanguage(new ScalaCompilerPlugin)
    }

    val functionZeroArgs2 =
      """class Foo {
           def bar() = true
           def baz() =
             this.bar _
         }
         """.stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(functionZeroArgs2).compileToLanguage(new ScalaCompilerPlugin)
    }
  }

  it should "support methods with type parameters" in {
    val methodWithTParam =
      """class Test[A] {
        |  def foo[B](a: B) = a
        |}
        |
        |class Bar {
        |  def t() = new Test[String]().foo(5)
        |}""".stripMargin

    assert(ProgramCompiler(methodWithTParam).compileToLanguage(new Z3CompilerPlugin)._2 != "")

    val methodWithExplicitTArg =
      """class Test[A] {
        |  def foo[B](a: B) = a
        |}
        |
        |class Bar {
        |  def t() = new Test[Int]().foo(5)
        |}""".stripMargin

    assert(ProgramCompiler(methodWithExplicitTArg).compileToLanguage(new Z3CompilerPlugin)._2 != "")

    val methodWithWrongExplicitTArg =
      """class Test[A] {
        |  def foo[B](a: B) = a
        |}
        |
        |class Bar {
        |  def t() = new Test[Boolean]().foo[String](5)
        |}""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(methodWithWrongExplicitTArg).compileToLanguage(new Z3CompilerPlugin)
    }
  }

  it should "support predicates with type parameters" in {
    val predWithTypeParam =
      """class Test[A] {
        |  pre foo[B](a: B) {
        |    true
        |  }
        |
        |  def foo[B](a: B) = a
        |}""".stripMargin

    assert(ProgramCompiler(predWithTypeParam).compileToLanguage(new Z3CompilerPlugin)._2 != "")

    // Throw an error if the predicate defines more or less type parameters than the method it is associated to
    val predWithWrongTypeParam =
      """class Test[A] {
        |  pre foo[B, C](a: B) {
        |    true
        |  }
        |
        |  def foo[B](a: B) = a
        |}""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(predWithWrongTypeParam).compileToLanguage(new Z3CompilerPlugin)
    }

    val predWithWrongTypeParam2 =
      """class Test[A] {
        |  pre foo[C](a: C) {
        |    true
        |  }
        |
        |  def foo[B](a: B) = a
        |}""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(predWithWrongTypeParam2).compileToLanguage(new Z3CompilerPlugin)
    }
  }
}
