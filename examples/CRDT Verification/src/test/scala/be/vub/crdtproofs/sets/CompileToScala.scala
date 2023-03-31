package be.vub.crdtproofs.sets

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Path, Paths}

import be.vub.verifx.Compiler.Plugins.{JavaScriptCompilerPlugin, ScalaCompilerPlugin}
import be.vub.verifx.Compiler.ProjectCompiler
import be.vub.verifx.Utilities.Scanner

object CompileToScala extends App {
  val vfxFiles = Scanner.scan
  val compiledFiles = ProjectCompiler(vfxFiles.toSet).compileToLanguage(new ScalaCompilerPlugin).map { case (fileName, code) => (ProjectCompiler.pkgToFilePath(fileName), code) }

  compiledFiles foreach {
    case (fileName, code) => {
      // Transform file path from src/main/verifx/<rest> to src/main/scala/<rest>
      val path: Path = Paths.get(fileName)
      val newPath = Paths.get(path.toAbsolutePath.normalize.toString.replace("/src/main/verifx/", "/src/main/scala/"))

      try {
        writeToFile(code, newPath)
      } catch {
        case e: Throwable => {
          println(s"Failed to write to $newPath")
          throw e
        }
      }
    }
  }

  private def writeToFile(content: String, path: Path): Unit = {
    val oldFile = path.toFile
    val newFileName = replaceExtension(oldFile.getAbsolutePath)
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

  private def replaceExtension(fileName: String): String = fileName.stripSuffix(".vfx") + ".scala"
}
