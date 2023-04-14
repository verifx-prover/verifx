package org.verifx.Compiler

import java.nio.file.Path
import ProgramCompiler.FileName
import org.verifx.Analysis.Proofs.{ProofName, ProofResult}
import Types.{Classes, Proof}
import IR.IR
import cats.data.State
import cats.implicits._
import org.verifx.Analysis.Proofs
import org.verifx.Compiler.Plugins.Z3CompilerPlugin
import org.verifx.ParseError

import scala.meta._

object ProgramCompiler {
  /**
   * Compiler for a string containing the code.
   */
  def apply(source: String): ProgramCompiler = makeProgramCompiler(source.parse[Source].get)

  /**
   * Compiler for a file.
   */
  def apply(file: Path): ProgramCompiler = {
    val bytes = java.nio.file.Files.readAllBytes(file)
    val text = new String(bytes, "UTF-8")
    val input = Input.VirtualFile(file.toString, text)
    makeProgramCompiler(input.parse[Source].get)
  }

  private def makeProgramCompiler(ast: Source): ProgramCompiler = {
    val stats = ast.stats
    val file = BuiltInClasses.builtInKinds ++ stats
    new ProgramCompiler(file)
  }

  private val imports = "import org.verifx.Compiler.IR._"
  type FileName = String

  // Transforms `import org.verifx.kdeporre.mypackage.myfile` to `IR.Import("be/vub/kdeporre/mypackage/myfile")`
  private def compileImport(stat: Import): String = {
    val pkgStr = stat.importers.mkString(".")
    s"""Import("$pkgStr")"""
  }

  def isProofClass(className: String): Boolean = className.matches("^__VFx_Proof\\d+__$") // matches __VFx_Proof<digits>__ where <digits> is a number
  def isProofClass(clazz: ClassOrTrait): Boolean = isProofClass(clazz.name.value)
  def getProofName(proof: Proof): ProofName = (proof.containerObject, proof.name)
}

case class ProgramCompiler(file: List[Stat], clazzes: Classes = Classes(), proofs: Set[Proof] = Set(), proofIndex: Map[ProofName, Proof] = Map(), filePath: Option[FileName] = None) {
  import ProgramCompiler._

  file.find(stat => !stat.isInstanceOf[Defn.Class] && !stat.isInstanceOf[Defn.Trait] && !stat.isInstanceOf[Import] && !stat.isInstanceOf[Defn.Object]) match {
    case Some(stat) => throw ParseError(s"Illegal statement $stat, only imports, classes, objects, and traits are allowed on top level.", stat.pos)
    case None => ()
  }

  lazy val compile: (Classes, String) = {
    val (classes, orderedClazzes, _, _) = classesAndKindsAndProofs
    (classes,
      imports + "\n\n" +
        orderedClazzes
          .map(_.compiled)
          .mkString("\n\n"))
  }

  val importStats: List[String] =
    file
      .filter(_.isInstanceOf[Import])
      .asInstanceOf[List[Import]]
      .map(compileImport)

  lazy val classesAndKindsAndProofs = {
    import CompilerUtil._ENUM_KEYWORD_

    val (classes, extendedProofs, extendedProofIndex) = file.foldLeft((clazzes, proofs, proofIndex)) {
      case ((classes, proofs, proofIndex), defn) => {
        defn match {
          case classDef: Defn.Class => {
            val clazz = ClassCompiler(classes).transformer(classDef)
            (classes + (classDef.name.value -> clazz), proofs, proofIndex)
          }
          case traitDef: Defn.Trait => {
            val clazz = ClassCompiler(classes).transformer(traitDef)
            (classes + (traitDef.name.value -> clazz), proofs, proofIndex)
          }
          case _: Import => (classes, proofs, proofIndex)
          case obj @ Defn.Object(_, _, templ @ Template(_, _, _, body)) => {
            // Compile objects in 2 steps.
            // 1) Compile the enumerations
            // 2) Compile the remaining methods and proofs

            // Filter the enumerations
            val (enumStats, rest) = body.partition(CompilerUtil.isEnum)
            val enums = enumStats.asInstanceOf[List[Term.ApplyInfix]]

            // Compile the enumerations one after the other (in order)
            val newClasses = enums.foldLeft(classes) {
              case (classes, enum) => EnumCompiler(classes).transformer(enum)
            }

            // Compile the remaining methods and proofs
            val objWithoutEnums = obj.copy(templ = templ.copy(stats = rest))
            val (newClasses2, newProofs) = ProofCompiler(proofs)(newClasses).transformer(objWithoutEnums)
            val extendedProofIndex = proofIndex ++ newProofs.map(proof => (getProofName(proof), proof))
            (newClasses2, proofs ++ newProofs, extendedProofIndex)
          }
        }
      }
    }

    // Keep a set of classes and traits defined in this file
    val definedInThisFile = file.filter(stat => stat.isInstanceOf[Defn.Class] || stat.isInstanceOf[Defn.Trait] || stat.isInstanceOf[Defn.Object]).map {
      case c: Defn.Class => List(c.name.value)
      case t: Defn.Trait => List(t.name.value)
      case obj: Defn.Object => {
        val res = obj
          .templ
          .stats
          .filter(CompilerUtil.isEnum)
          .asInstanceOf[List[Term.ApplyInfix]]
          .map(CompilerUtil.getEnumName)
        res
      }
    }.flatten.toSet

    val classesInOrder =
      classes
        .classDefinitionOrder
        .iterator
        .filter(classOrTraitOrEnum => definedInThisFile.contains(classOrTraitOrEnum.name.value)) // only keep the classes that are defined in this file
        .toList

    (classes, classesInOrder, extendedProofs, extendedProofIndex)
  }

  lazy val newClasses = classesAndKindsAndProofs._1
  lazy val newProofs = classesAndKindsAndProofs._3
  lazy val newProofIndex = classesAndKindsAndProofs._4

  private def isBuiltInClass(clazz: ClassOrTraitOrEnum, plugin: Plugin[_, _, _]): Boolean = {
    val builtInClasses = if (plugin.isInstanceOf[Z3CompilerPlugin]) BuiltInClasses.builtInClassNamesZ3 else BuiltInClasses.builtInClassNames
    val className = clazz.name.value
    builtInClasses.contains(className)
  }

  private def compilationState[C, M <: C, S](plugin: Plugin[C, M, S]): State[S, List[C]] = {
    import scala.reflect.runtime.currentMirror
    import scala.tools.reflect.ToolBox

    val toolbox = currentMirror.mkToolBox()

    val irClasses =
      classesAndKindsAndProofs._2
        .filter(clazz => !isBuiltInClass(clazz, plugin)) // do not compile built-in classes such as Set and Map to the target language
        .map(clazz => clazz.compiled)

    val irProofs =
      if (plugin.isInstanceOf[Z3CompilerPlugin])
        classesAndKindsAndProofs._3.map(_.compiled)
      else
        Set() // Only compile proofs to Z3

    val irProgram =
      (importStats ++ irClasses ++ irProofs)
        .map(ir =>
          s"""$imports
             |$ir""".stripMargin)

    val compiler = new IR2LanguageCompiler(plugin)

    val compileBody = irProgram traverse {
      irStat => {
        val ir = toolbox.eval(toolbox.parse(irStat)).asInstanceOf[IR]
        compiler.compile(ir)
      }
    }

    // When compiling the file, first call `plugin.startFile`
    // then compile the body and then call `plugin.endFile`
    for {
      start <- plugin.startFile(filePath)
      body <- compileBody
      end <- plugin.endFile()
    } yield (start :: body) ++ List(end)
  }

  def compileToLanguage[C, M <: C, S](plugin: Plugin[C, M, S], initialState: Option[S] = None): (S, String) = {
    val compilation = compilationState(plugin)
    plugin.run(compilation, initialState)
  }

  def compileToZ3() = {
    val plugin = new Z3CompilerPlugin
    val compilation = compilationState(plugin)
    plugin.run(compilation, None)
  }

  private def getProof(proofName: ProofName): (Types.Proof, Z3CompilerPlugin.CompilerState, String) = {
    val (compilerState, compiledProgram) = compileToZ3
    newProofIndex.get(proofName) match {
      case Some(proof) => (proof, compilerState, compiledProgram)
      case None => throw new Error(s"Proof $proofName not found.")
    }
  }

  def checkProof(proofName: ProofName, maxTries: Int = 5, timeout: Int = 10000): ProofResult = {
    val (proof, compilerState, compiledProgram) = getProof(proofName)
    Proofs.checkProof(proof, compiledProgram, newClasses, compilerState, maxTries, timeout)
  }

  def checkProofForModel(proofName: ProofName, maxTries: Int = 5, timeout: Int = 10000): ProofResult = {
    val (proof, compilerState, compiledProgram) = getProof(proofName)
    Proofs.checkProofForModel(proof, compiledProgram, newClasses, compilerState, maxTries, timeout)
  }

  def checkProofs(maxTries: Int = 5, timeout: Int = 10000): Map[ProofName, ProofResult] = {
    val (compilerState, compiledProgram) = compileToZ3
    Proofs.checkProofs(newProofs, compiledProgram, newClasses, compilerState, maxTries, timeout)
  }
}
