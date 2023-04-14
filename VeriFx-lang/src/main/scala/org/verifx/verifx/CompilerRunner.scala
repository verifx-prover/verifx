package org.verifx.verifx

import java.lang.Error
import java.nio.file.{Path, Paths}
import java.io.{BufferedWriter, File, FileWriter}
import org.verifx.verifx.Utilities.Scanner
import org.verifx.verifx.Compiler.ProjectCompiler
import org.verifx.verifx.Compiler.Plugins.{JavaScriptCompilerPlugin, ScalaCompilerPlugin, Z3CompilerPlugin}
import org.verifx.verifx.Compiler.ProjectCompiler
import org.verifx.verifx.Utilities.Scanner

/**
 * Scans the `src/main/verifx/` directory for VeriFx files
 * and transpiles them to the requested language in the `src/main/lang/` output directory.
 */
object CompilerRunner {
  def main(args: Array[String]): Unit = {
    // Parse the command line arguments
    if (args.size < 1) throw new Error("Target language not specified. Please provide the target language as a command-line argument.")
    val language = args.head
    if (args.size == 1)
      compileTo(language)()
    else if (args.size == 2) {
      val out = args(1) // output directory
      compileTo(language)(Some(out))
    }
    else
      throw new Error(s"Expected 1 or 2 command-line arguments but got ${args.size}")
  }

  def compileTo(language: String)(out: Option[String] = None): Unit = {
    val vfxFiles = Scanner.scan.toSet
    val compiler: ProjectCompiler = ProjectCompiler(vfxFiles)

    val (compiled, fileExtension) = language match {
      case "z3" =>
        (compiler.compileToLanguage(new Z3CompilerPlugin), "smt")
      case "scala" =>
        (compiler.compileToLanguage(new ScalaCompilerPlugin), "scala")
      case "js" =>
        (compiler.compileToLanguage(new JavaScriptCompilerPlugin), "js")
      case _ => throw new Error(s"Did not find compiler for $language")
    }

    // Compile to the target language and add the full path to the file names
    val compiledFiles = compiled.map { case (fileName, code) => (ProjectCompiler.pkgToFilePath(fileName), code) }
    // Write each file to disk
    compiledFiles foreach {
      case (fileName, code) => {
        // Transform file path from src/main/verifx/<rest> to src/main/scala/<rest>
        val path: Path = Paths.get(fileName)
        val newPath = out match {
          case Some(outPath) => {
            val outDir = if (outPath.endsWith("/")) outPath.stripSuffix("/") else outPath
            val relativePathToFile = Paths.get("src/main/verifx/").toAbsolutePath.normalize().relativize(path.toAbsolutePath.normalize())
            Paths.get(s"$outDir/$relativePathToFile")
          }
          case None =>
            Paths.get(path.toAbsolutePath.normalize.toString.replace("/src/main/verifx/", s"/src/main/$language/"))
        }

        try {
          writeToFile(code, newPath, fileExtension)
        } catch {
          case e: Throwable => {
            println(s"Failed to write to $newPath")
            throw e
          }
        }
      }
    }
  }

  private def writeToFile(content: String, path: Path, fileExtension: String): Unit = {
    val oldFile = path.toFile
    val newFileName = replaceExtension(oldFile.getAbsolutePath, fileExtension)
    val file = new File(newFileName)

    def write(): Unit = {
      val fw = new FileWriter(file.getAbsoluteFile())
      val bw = new BufferedWriter(fw)
      bw.write(content)
      bw.close()
    }

    // Write compiled code to the new path
    if (file.exists) {
      // Ask permission to override
      scala.io.StdIn.readLine(s"${file.getPath} already exists, overwrite it [y/n] ? ") match {
        case "y" => write()
        case _ =>
      }
    }
    else {
      // Create the entire path
      file.getParentFile().mkdirs()
      write()
    }
  }

  private def replaceExtension(fileName: String, newFileExtension: String): String = fileName.stripSuffix(".vfx") + s".$newFileExtension"
}
