import org.verifx.verifx.Compiler.Plugins.{JavaScriptCompilerPlugin, ScalaCompilerPlugin, Z3CompilerPlugin}
import org.verifx.verifx.Compiler.ProgramCompiler
import org.verifx.verifx.{ParseError, TypeError}
import org.scalatest.FlatSpec

class EnumTest extends FlatSpec {
  "Enumerations" should "be supported" in {
    val enumeration =
      """object Op_enum {
        |  enum Op[V] {
        |    Ins(pos: Int, c: V) | Del(pos: Int) | Id()
        |  }
        |}""".stripMargin

    assert(ProgramCompiler(enumeration).compileToLanguage(new ScalaCompilerPlugin)._2 != "")

    val enumeration2 =
      """object Op_enum {
        |  enum Op[V] {
        |    Ins(pos: Int, c: V)
        |  }
        |}""".stripMargin

    assert(ProgramCompiler(enumeration2).compileToLanguage(new ScalaCompilerPlugin)._2 != "")
  }

  it should "support type casts" in {
    // We should be able to type cast an enumeration type to a specific data constructor
    val correctTypeCast1 =
      """object Op_enum {
        |  enum Op[V] {
        |    Ins(pos: Int, c: V) | Del(pos: Int) | Id()
        |  }
        |}
        |
        |class Foo {
        |  def getC(d: Op[String]) = d.asInstanceOf[Del[String]].pos
        |}""".stripMargin

    assert(ProgramCompiler(correctTypeCast1).compileToLanguage(new ScalaCompilerPlugin)._2 != "")

    // We should be able to type cast an data constructor to its enumeration type
    val correctTypeCast2 =
      """object Op_enum {
        |  enum Op[V] {
        |    Ins(pos: Int, c: V) | Del(pos: Int) | Id()
        |  }
        |}
        |
        |class Foo {
        |  def getC(i: Ins[String]) = i.asInstanceOf[Op[String]]
        |}""".stripMargin

    assert(ProgramCompiler(correctTypeCast2).compileToLanguage(new ScalaCompilerPlugin)._2 != "")

    // We should be able to type cast a wrapped data constructor to its wrapped enumeration type
    val correctTypeCast3 =
      """object Op_enum {
        |  enum Op[V] {
        |    Ins(pos: Int, c: V) | Del(pos: Int) | Id()
        |  }
        |}
        |
        |class Foo {
        |  def getC(i: Set[Op[String]]) = i.asInstanceOf[Set[Ins[String]]]
        |}""".stripMargin

    assert(ProgramCompiler(correctTypeCast3).compileToLanguage(new ScalaCompilerPlugin)._2 != "")

    // Objects that are not enumerations should not support type casts
    val wrongTypeCast1 =
      """object Op_enum {
        |  enum Op[V] {
        |    Ins(pos: Int, c: V) | Del(pos: Int) | Id()
        |  }
        |}
        |
        |class Test[V]
        |
        |class Foo {
        |  def getC(i: Test[String]) = i.asInstanceOf[Op[String]]
        |}""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(wrongTypeCast1).compileToLanguage(new ScalaCompilerPlugin)
    }

    val wrongTypeCast2 =
      """object Op_enum {
        |  enum Op[V] {
        |    Ins(pos: Int, c: V) | Del(pos: Int) | Id()
        |  }
        |}
        |
        |class Test[V]
        |
        |class Foo {
        |  def getC(i: Test[String]) = i.asInstanceOf[Ins[String]]
        |}""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(wrongTypeCast2).compileToLanguage(new ScalaCompilerPlugin)
    }

    // It should not be possible to type cast to a type that is not an enumeration or data constructor
    val wrongTypeCast3 =
      """object Op_enum {
        |  enum Op[V] {
        |    Ins(pos: Int, c: V) | Del(pos: Int) | Id()
        |  }
        |}
        |
        |class Test[V]
        |
        |class Foo {
        |  def getC(i: Op[String]) = i.asInstanceOf[String]
        |}""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(wrongTypeCast3).compileToLanguage(new ScalaCompilerPlugin)
    }
  }

  it should "throw an error if syntax is wrong" in {
    val enumeration =
      """object Op_enum {
        |  enum Op[V] {
        |    class Ins(pos: Int, c: V)
        |  }
        |}""".stripMargin

    assertThrows[ParseError] {
      ProgramCompiler(enumeration).compileToLanguage(new ScalaCompilerPlugin)
    }
  }

  it should "transpile to Z3" in {
    val enumeration =
      """object Op_enum {
        |  enum Op[V, W] {
        |    Ins(pos: Int, c: V, y: W) | Del(pos: Int, y: W) | Id()
        |  }
        |}
        |
        |class Test {
        |  def foo(): Ins[String, Int] = {
        |    new Ins(5, "abc", 9)
        |  }
        |  def bar(): Del[String, Boolean] = {
        |    new Del[String, Boolean](6, true)
        |  }
        |}
        |""".stripMargin

    ProgramCompiler(enumeration).compileToLanguage(new Z3CompilerPlugin)._2
  }
}
