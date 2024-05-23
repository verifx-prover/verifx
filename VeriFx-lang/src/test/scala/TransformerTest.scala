import org.verifx.Compiler.IR.Defn.Defn
import org.verifx.Compiler.IR.{Defn, IR, Term}
import org.verifx.Compiler.Plugins.ScalaCompilerPlugin
import org.verifx.Compiler.ProgramCompiler
import org.scalatest.FlatSpec

class TransformerTest extends FlatSpec {
  "Compiler AST transformations" should "support chopping methods" in {
    val testProgram =
      """class Test {
           def foo() = {
             val a = 5
             val b = 6
             a+b
           }
         }""".stripMargin

    val choppedScalaProgram =
      ProgramCompiler(testProgram)
        .compileToLanguage(
          new ScalaCompilerPlugin,
          (IR) => IR match {
            case classDef: Defn.ClassDef => {
              val methods = classDef.methods
              val chopMethod = (m: Defn.MethodOrPredicate) => {
                m match {
                  case Defn.MethodDef(name, mods, tparams, args, body, retType, isRecursive) => {
                    body match {
                      case Term.Block(terms) => {
                        terms.map(term => {
                          Defn.MethodDef(name, mods, tparams, args, Term.Block(List(term)), retType, isRecursive)
                        })
                      }
                      case term => List(term)
                    }
                  }
                  case ir => List(ir)
                }
              }
              val choppedMethods = methods.flatMap(chopMethod).asInstanceOf[List[Defn.MethodOrPredicate]]
              classDef.copy(methods = choppedMethods)
            }
            case ir => ir
          }
        )

    val expected =
      """import org.verifx.Runtime.Implicits._
        |
        |case class Test() {
        |   def foo(): Int = {
        |     val a: Int = 5
        |   }
        |
        |   def foo(): Int = {
        |     val b: Int = 6
        |   }
        |
        |   def foo(): Int = {
        |     (a + b)
        |   }
        | }
        |
        |""".stripMargin

    def normalized(s: String): String = s.replaceAll("\\s+", " ") // replaces consecutive white spaces by one in order to ignore differences in whitespace when matching below
    assert(normalized(choppedScalaProgram._2) == normalized(expected))
  }
}
