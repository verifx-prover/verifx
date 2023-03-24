import be.vub.kdeporre.verifx.Compiler.Plugins.{JavaScriptCompilerPlugin, ScalaCompilerPlugin, Z3CompilerPlugin}
import be.vub.kdeporre.verifx.Compiler.ProgramCompiler
import be.vub.kdeporre.verifx.{MatchError, ParseError, TypeError}
import org.scalatest.FlatSpec

class PatternMatchTest extends FlatSpec {
  "Pattern match" should "be supported on enumerations" in {
    val pm =
      s"""object Op_enum {
         |  enum Op[V] {
         |    Ins(pos: Int, c: V) | Del(pos: Int) | Id()
         |  }
         |}
         |
         |class Foo {
         |  def bar[K](op: Op[K]) = op match {
         |    case i: Ins[K] => {
         |      val a = true
         |      val b = true
         |      a && b
         |    }
         |    case d: Del[K] => false
         |    case i: Id[K] => false
         |  }
         |}""".stripMargin

    assert(ProgramCompiler(pm).compileToLanguage(new ScalaCompilerPlugin)._2 != "")
  }

  it should "support typed patterns with wildcard variable" in {
    val pm =
      s"""object Op_enum {
         |  enum Op[V] {
         |    Ins(pos: Int, c: V) | Del(pos: Int) | Id()
         |  }
         |}
         |
         |class Foo {
         |  def isDelete[K](op: Op[K]) = op match {
         |    case _: Del[K] => true
         |    case _: Ins[K] => false
         |    case _: Id[K] => false
         |  }
         |}""".stripMargin

    assert(ProgramCompiler(pm).compileToLanguage(new ScalaCompilerPlugin)._2 != "")
  }

  it should "support wildcard patterns" in {
    val pm =
      s"""object Op_enum {
         |  enum Op[V] {
         |    Ins(pos: Int, c: V) | Del(pos: Int) | Id()
         |  }
         |}
         |
         |class Foo {
         |  def isDelete[K](op: Op[K]) = op match {
         |    case d: Del[K] => true
         |    case _ => false
         |  }
         |}""".stripMargin

    assert(ProgramCompiler(pm).compileToLanguage(new ScalaCompilerPlugin)._2 != "")
  }

  it should "support extraction patterns" in {
    val pm =
      s"""object Op_enum {
         |  enum Op[V] {
         |    Ins(pos: Int, c: V) | Del(pos: Int) | Id()
         |  }
         |}
         |
         |class Foo {
         |  def getPos[K](op: Op[K]) = op match {
         |    case Ins(pos, c) => pos
         |    case Del(pos) => pos
         |    case Id() => -1
         |  }
         |}""".stripMargin

    assert(ProgramCompiler(pm).compileToLanguage(new Z3CompilerPlugin)._2 != "")
  }

  it should "throw an error if wildcard pattern is not the last one" in {
    val pm =
      s"""object Op_enum {
         |  enum Op[V] {
         |    Ins(pos: Int, c: V) | Del(pos: Int) | Id()
         |  }
         |}
         |
         |class Foo {
         |  def isDelete[K](op: Op[K]) = op match {
         |    case d: Del[K] => true
         |    case _ => false
         |    case i: Ins[K] => false
         |  }
         |}""".stripMargin

    assertThrows[ParseError] {
      ProgramCompiler(pm).compileToLanguage(new ScalaCompilerPlugin)
    }
  }

  it should "introduce constants in patterns in the scope of the body" in {
    val pm =
      s"""object Op_enum {
         |  enum Op[V] {
         |    Ins(pos: Int, c: V) | Del(pos: Int) | Id()
         |  }
         |}
         |
         |class Foo {
         |  def bar[K](op: Op[K]) = op match {
         |    case Ins(pos, _) => pos == 5
         |    case d: Del[K] => false
         |    case i: Id[K] => false
         |  }
         |}""".stripMargin

    assert(ProgramCompiler(pm).compileToLanguage(new ScalaCompilerPlugin)._2 != "")
  }

  it should "fail if expression is not an enumeration" in {
    val pm =
      s"""object Op_enum {
         |  enum Op[V] {
         |    Ins(pos: Int, c: V) | Del(pos: Int) | Id()
         |  }
         |}
         |
         |class Test
         |
         |class Foo {
         |  def bar[K](op: Test) = op match {
         |    case i: Ins[K] => true
         |    case d: Del[K] => false
         |    case i: Id[K] => false
         |  }
         |}""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(pm).compileToLanguage(new ScalaCompilerPlugin)
    }
  }

  it should "support cases with different subtypes but common super type in hierarchy" in {
    // The below example should throw an error because the pattern match expression has type `Bar`
    // but this is a trait type and traits don't exist in Z3.
    val pmSubtypes =
      """trait Foo

         trait Bar extends Foo
         trait Baz extends Bar

         class C1 extends Bar
         class C2 extends Baz

         object T {
           enum Op {
             Ins(pos: Int, c: String) | Del()
           }
         }

         class Test {
           def foo(a: Op) = {
             a match {
               case _: Ins => new C1()
               case _: Del => new C2()
             }

             true
           }
         }""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(pmSubtypes).compileToLanguage(new ScalaCompilerPlugin)
    }

    // In the below example the pattern match evaluates to something of type `Op`.
    // This is allowed because `Op` is an enumeration and enumerations exist in Z3.
    val pmSubtypesCorrect =
      """object T {
           enum Op {
             Ins(pos: Int, c: String) | Del()
           }
         }

         class Test {
           def foo(a: Op) = {
             a match {
               case i: Ins => i
               case d: Del => d
             }
           }
         }""".stripMargin

    assert(ProgramCompiler(pmSubtypesCorrect).compileToLanguage(new ScalaCompilerPlugin)._2 != "")
  }

  it should "fail if cases have different return types" in {
    val pm =
      s"""object Op_enum {
         |  enum Op[V] {
         |    Ins(pos: Int, c: V) | Del(pos: Int) | Id()
         |  }
         |}
         |
         |class Foo {
         |  def bar[K](op: Op[K]) = op match {
         |    case i: Ins[K] => true
         |    case d: Del[K] => false
         |    case i: Id[K] => 7
         |  }
         |}""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(pm).compileToLanguage(new ScalaCompilerPlugin)
    }
  }

  it should "fail if case will never match" in {
    val pm =
      s"""object Op_enum {
         |  enum Op[V] {
         |    Ins(pos: Int, c: V) | Del(pos: Int) | Id()
         |  }
         |}
         |
         |class Foo {
         |  def bar[K](op: Op[K]) = op match {
         |    case i: Ins[K] => true
         |    case d: Del[Int] => false
         |    case i: Id[K] => false
         |  }
         |}""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(pm).compileToLanguage(new ScalaCompilerPlugin)
    }
  }

  it should "fail if it is not exhaustive" in {
    val pm =
      s"""object Op_enum {
         |  enum Op[V] {
         |    Ins(pos: Int, c: V) | Del(pos: Int) | Id()
         |  }
         |}
         |
         |class Foo {
         |  def bar[K](op: Op[K]) = op match {
         |    case i: Ins[K] => true
         |    case d: Del[K] => false
         |  }
         |}""".stripMargin

    assertThrows[MatchError] {
      ProgramCompiler(pm).compileToLanguage(new ScalaCompilerPlugin)
    }
  }

  it should "fail if pattern is malformed" in {
    val pm =
      s"""object Op_enum {
         |  enum Op[V] {
         |    Ins(pos: Int, c: V) | Del(pos: Int) | Id()
         |  }
         |}
         |
         |class Foo {
         |  def bar[K](op: Op[K]) = op match {
         |    case i: Ins[K] => true
         |    case d: Del[K] if 1 == 2 => false
         |    case i: Id[K] => false
         |  }
         |}""".stripMargin

    assertThrows[ParseError] {
      ProgramCompiler(pm).compileToLanguage(new ScalaCompilerPlugin)
    }
  }

  it should "compile to Z3 correctly" in {
    val pm =
      s"""object Op_enum {
         |  enum Op[V] {
         |    Ins(pos: Int, c: V) | Del(pos: Int) | Id()
         |  }
         |}
         |
         |class Test[V] {
         |  def posChange[K](op: Op[K]) = op match {
         |    case i: Ins[K] => i.pos + 1
         |    case d: Del[K] => d.pos - 1
         |    case i: Id[K] => 0
         |  }
         |
         |  def isInsert(op: Op[String]) = op match {
         |    case _: Ins[String] => true
         |    case _ => false
         |  }
         |
         |  def foo() = this.posChange(new Ins(5, "abc"))
         |}
         |
         |/*
         |class Bar {
         |  def bar() = (new Test[Int]).foo()
         |}*/
         |""".stripMargin

    val res = ProgramCompiler(pm).compileToLanguage(new Z3CompilerPlugin)
    assert(res._2 != "")
  }
}
