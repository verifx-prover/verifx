import org.scalatest.FlatSpec
import org.verifx.Compiler.Plugins.ScalaCompilerPlugin
import org.verifx.Compiler.ProjectCompiler

class ImportTest extends FlatSpec {
  "Imports" should "make all definitions of the imported file visible in Scala" in {
    // `import org.example.shapes.Shapes` imports the file Shapes.vfx, which defines more than a class `Shapes`
    val shapes =
      """class Shapes(size: Int)
        |class Circle(radius: Int)
        |object Kind_enum {
        |  enum Kind {
        |    Round() | Square()
        |  }
        |}""".stripMargin

    val drawing =
      """import org.example.shapes.Shapes
        |
        |class Drawing(circle: Circle, kind: Kind) {
        |  def isRound() = this.kind == new Round()
        |}""".stripMargin

    val project = ProjectCompiler(Map(
      "src/main/verifx/org/example/shapes/Shapes.vfx" -> shapes,
      "src/main/verifx/org/example/drawing/Drawing.vfx" -> drawing))

    val compiled = project.compileToLanguage(new ScalaCompilerPlugin).toMap
    val drawingCode = compiled.collectFirst { case (file, code) if file.endsWith("Drawing") => code }.get
    assert(drawingCode.contains("import org.example.shapes.{Circle, Kind, Round, Shapes, Square}"))
  }
}
