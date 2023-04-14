import org.scalatest.FlatSpec
import org.verifx.Compiler.Plugins.{ScalaCompilerPlugin, Z3CompilerPlugin}
import org.verifx.Compiler.ProgramCompiler
import org.verifx.TypeError

class ObjectTest extends FlatSpec {
  "Objects" should "be able to extend traits" in {
    val obj =
      """trait Bar {
        |  def bar(): Int
        |  def foo(): Int = this.bar() + 1
        |}
        |object Foo extends Bar {
        |  def bar(): Int = 4
        |  proof test {
        |    this.foo() == 5
        |  }
        |}""".stripMargin

    val res = ProgramCompiler(obj).compileToLanguage(new Z3CompilerPlugin)._2
    assert(res != "")
  }

  it should "throw an error if object does not provide required fields/methods" in {
    val obj =
      """trait Bar {
        |  def bar(): Int
        |  def foo(): Int = this.bar() + 1
        |}
        |object Foo extends Bar {
        |  proof test {
        |    this.foo() == 5
        |  }
        |}""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(obj).compileToLanguage(new ScalaCompilerPlugin)._2
    }
  }

  it should "inherit proofs from super trait" in {
    val traitWithProof =
      """trait Foo {
           def foo(): Int
           def baz() = this.foo() + 1

           proof bar {
             this.baz() == this.foo() + 1
           }
         }

         object Test extends Foo {
           def foo(): Int = 4
         }
      """.stripMargin

    assert(ProgramCompiler(traitWithProof).compileToLanguage(new Z3CompilerPlugin)._2 != "")
  }

  it should "inherit proofs from traits with type params" in {
    val obj =
      """trait Bar[V] {
        |  def test(x: V): V
        |  proof baz {
        |    forall (v: V) {
        |      this.test(v) == this.test(v)
        |    }
        |  }
        |}
        |object Foo extends Bar[Int] {
        |  def test(x: Int): Int = x+1
        |}""".stripMargin

    val res = ProgramCompiler(obj).compileToLanguage(new Z3CompilerPlugin)._2
    assert(res != "")
  }
}
