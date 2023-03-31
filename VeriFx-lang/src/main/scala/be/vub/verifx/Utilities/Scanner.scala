package be.vub.verifx.Utilities

import java.io.File
import java.nio.file.{Path, Paths}

object Scanner {
  private def vfxExtension = ".vfx"

  /**
   * Scans the `src/main/verifx/` directory for `*.vfx` files.
   */
  def scan(): List[Path] = {
    val dir = new File("src/main/verifx")
    val files = getListOfFiles(dir)
    files
      .filter(_.getName.endsWith(vfxExtension)) // only keep .vfx files
      .map(f => Paths.get(f.getPath))
  }

  /**
   * Returns a list of all files in the given directory and its subdirectories
   */
  private def getListOfFiles(dir: File): List[File] = {
    val filesInDir = dir.listFiles.toList
    filesInDir.filter(_.isFile) ++ filesInDir.filter(_.isDirectory).flatMap(getListOfFiles)
  }
}
