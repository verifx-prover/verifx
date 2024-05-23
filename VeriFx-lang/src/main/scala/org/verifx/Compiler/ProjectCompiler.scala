package org.verifx.Compiler

import java.io.File
import java.nio.file.Path

import org.verifx.Analysis.Proofs.{Aborted, ProofName, ProofResult, Proved, Rejected}
import ProgramCompiler.FileName
import ProjectCompiler.{ClassName, ObjectName}
import Types.{Classes, Proof}
import org.jgrapht.alg.connectivity.ConnectivityInspector
import org.jgrapht.graph.{DefaultEdge, DirectedAcyclicGraph}
import org.verifx.ParseError

import scala.meta._

object ProjectCompiler {
  type ClassName = String
  type ObjectName = String

  /**
   * Compiles a map of file names to their code.
   */
  def apply(sources: Map[FileName, String]): ProjectCompiler = {
    import scala.collection.parallel.CollectionConverters._

    // Parse the files in parallel
    val parsedFiles = sources.par.map {
      case (fileName, src) =>
        (filePathToPkg(fileName), src.parse[Source].get)
    }.seq
    makeProjectCompiler(parsedFiles)
  }

  /**
   * Compiles a set of files.
   */
  def apply(files: Set[Path]): ProjectCompiler = {
    import scala.collection.parallel.CollectionConverters._

    // Parse the files in parallel
    val parsedFiles = files.par.map {
      case file => {
        val bytes = java.nio.file.Files.readAllBytes(file)
        val text = new String(bytes, "UTF-8")
        val input = Input.VirtualFile(file.toString, text)
        (filePathToPkg(file.toString), input.parse[Source].get)
      }
    }.seq

    makeProjectCompiler(parsedFiles.toMap)
  }

  def filePathToPkg(path: String) = path.stripPrefix(Path.of("src/main/verifx/").toString ++ s"${File.separatorChar}").stripSuffix(".vfx").replace(File.separatorChar, '.')
  def pkgToFilePath(path: String) = Path.of("src/main/verifx/").toString ++ s"${File.separatorChar}" ++ path.replace('.', File.separatorChar) ++ ".vfx"

  private def makeSubgraphOfDAG[V](g: DirectedAcyclicGraph[V, DefaultEdge], vertexSet: Set[V]): DirectedAcyclicGraph[V, DefaultEdge] = {
    import scala.jdk.CollectionConverters._

    val subgraph: DirectedAcyclicGraph[V, DefaultEdge] = new DirectedAcyclicGraph(classOf[DefaultEdge])
    vertexSet.foreach(subgraph.addVertex)
    val edges = vertexSet.map(g.edgesOf(_).asScala.filter(e => vertexSet.contains(g.getEdgeSource(e)) && vertexSet.contains(g.getEdgeTarget(e)))).flatten
    edges.foreach(edge => subgraph.addEdge(g.getEdgeSource(edge), g.getEdgeTarget(edge)))
    subgraph
  }

  private def computeFileOrder(sources: Map[String, Source]) = {
    import scala.jdk.CollectionConverters._

    // Make a DAG of dependencies between files
    val dependencyGraph: DirectedAcyclicGraph[FileName, DefaultEdge] = new DirectedAcyclicGraph(classOf[DefaultEdge])

    sources foreach {
      case (fileName, src) => {
        dependencyGraph.addVertex(fileName)
        val imports = src.stats.filter(_.isInstanceOf[Import]).map(imp => {
          val filePath = imp.toString.stripPrefix("import").trim
          if (!sources.contains(filePath)) throw ParseError(s"File $filePath not found.", imp.pos)
          filePath
        }).toSet

        imports foreach {
          case importt => {
            dependencyGraph.addVertex(importt)
            try {
              dependencyGraph.addEdge(importt, fileName) // add an edge from the imported file to this file, because the imported file must be compiled before this file
            } catch {
              case _: IllegalArgumentException => throw new Error(s"Could not compile project due to circular dependencies.\n$importt in $fileName introduces a cycle.")
            }
          }
        }
      }
    }

    val ci = new ConnectivityInspector(dependencyGraph)
    val components = ci.connectedSets().iterator().asScala
    // create a subgraph for each component
    val subgraphs = components.map(vertices => {
      // Copy the vertices and its edges to the new DAG
      makeSubgraphOfDAG(dependencyGraph, vertices.asScala.toSet)
    })
    // Topologically sort each component
    val componentFileOrders = subgraphs.map(g => g.iterator().asScala.map(fileName => (fileName, sources.get(fileName).get.stats)).toList).toList
    (componentFileOrders, dependencyGraph)
  }

  /**
   * Makes a compiler for an entire project
   * i.e. compiles several files
   * @param sources A map from file name to source code
   */
  def makeProjectCompiler(sources: Map[String, Source]) = {
    val (components, dependencyGraph) = computeFileOrder(sources)
    new ProjectCompiler(components, dependencyGraph, sources)
  }

  private def getClassNames(stats: List[Stat]) = {
    stats
      .filter(_.isInstanceOf[Defn.Class])
      .map(_.asInstanceOf[Defn.Class].name.value)
      .toSet
  }

  private def getProofNames(stats: List[Stat]) = {
    val objects = stats.view.filter(_.isInstanceOf[Defn.Object]).map(_.asInstanceOf[Defn.Object])
    objects.map {
      case Defn.Object(_, Term.Name(objectName), Template(_, _, _, stats)) => {
        val proofNames = stats.view.filter(ProofCompiler.isProof).map(ProofCompiler.getProofName)
        proofNames.map((objectName, _)) // combine the object's name and the proof name
      }
    }.flatten
  }

  private def getObjectNames(stats: List[Stat]) = {
    stats
      .filter(_.isInstanceOf[Defn.Object])
      .map(_.asInstanceOf[Defn.Object].name.value)
      .toSet
  }

  private def addBuiltInKinds(filesInOrder: List[(FileName, List[Stat])]): List[(FileName, List[Stat])] = {
    filesInOrder.head match {
      case (firstFileName, firstStats) => {
        val firstFile = BuiltInClasses.builtInKinds ++ firstStats
        (firstFileName, firstFile) :: filesInOrder.tail
      }
    }
  }

  private def glueFiles(files: List[(FileName, List[Stat])]): List[Stat] = files.view.map(_._2).flatten.toList
}

/**
 * Compiles a project comprising of several files by compiling each file
 * with the state of the previously compiled file.
 * @param components Files in order of their dependencies per subproject, i.e. A comes before B if B depends on A.
 *                   Note that a project may contain independent files, e.g. files A, B, C and A is standalone and B depends on A.
 *                   Than the project consists of 2 independent subprojects (A is a subproject, and B and C is a subproject).
 *                   Subprojects are compiled in parallel.
 * @param dependencyGraph Graph of file dependencies. There is an edge from file A to file B if B depends on A (i.e. A needs to be compiled before B).
 * @param sources Maps file names to their source code.
 */
case class ProjectCompiler(components: List[List[(FileName, List[Stat])]], dependencyGraph: DirectedAcyclicGraph[FileName, DefaultEdge], sources: Map[String, Source]) {
  import ProjectCompiler.{getClassNames, getProofNames, getObjectNames, addBuiltInKinds, glueFiles}

  // A list containing the files per component
  private lazy val filesPerComponent = components.map(addBuiltInKinds)

  // Index that maps class names to the file they are defined in
  private lazy val classIndex: Map[ClassName, FileName] = components.view.flatten.map {
    case (fileName, stats) => {
      val classNames = getClassNames(stats)
      classNames.view.map((_, fileName))
    }
  }.flatten.toMap

  private lazy val objectIndex: Map[ObjectName, FileName] = components.view.flatten.map {
    case (fileName, stats) => {
      val objectNames = getObjectNames(stats)
      objectNames.map((_, fileName))
    }
  }.flatten.toMap

  /*
  private lazy val proofIndex: Map[ProofName, FileName] = components.view.flatten.map {
    case (fileName, stats) => {
      val proofNames = getProofNames(stats)
      proofNames.map((_, fileName))
    }
  }.flatten.toMap
  */

  def compileToLanguage[C, M <: C, S](plugin: Plugin[C, M, S], transformer: IR.IR => IR.IR = identity[IR.IR]): List[(FileName, String)] = {
    import scala.collection.parallel.CollectionConverters._

    // Compile independent components in parallel
    filesPerComponent.par.map {
      case filesInOrder => {
        // Fold through the files and pass the previous state to the compiler as well as the previously defined classes,
        // and accumulate the compiled files
        filesInOrder.foldLeft((Option.empty[S], Classes(), Set.empty[Proof], Map.empty[ProofName, Proof], List.empty[(FileName, String)])) {
          case ((state, classes, proofs, proofIndex, compiledFiles), (fileName, stats)) => {
            val fileCompiler = new ProgramCompiler(stats, classes, proofs, proofIndex, Some(fileName))
            val (newState, compiledFile) = fileCompiler.compileToLanguage(plugin, transformer, state)
            val newFiles = (fileName, compiledFile) :: compiledFiles
            (Some(newState), fileCompiler.newClasses, fileCompiler.newProofs, fileCompiler.newProofIndex, newFiles)
          }
        }._5.reverse
      }
    }.flatten.toList
  }

  /**
   * Compiles a single file from this project to the specified target language.
   * @param file File to compile.
   * @param plugin Plugin to use for the compilation.
   * @return The compiled code.
   */
  def compileFileToLanguage[C, M <: C, S](file: FileName, plugin: Plugin[C, M, S]): String = {
    val filesInOrder = getRelevantFiles(file)
    val allFiles = addBuiltInKinds(filesInOrder)
    val gluedFile = glueFiles(allFiles)
    ProgramCompiler(gluedFile).compileToLanguage(plugin)._2
  }

  /**
   * Computes all files that are needed to compile this file,
   * i.e. computes all dependencies of this file.
   * @param fileName The name of the file we want to compile.
   * @return A list containing this file and its dependencies, in the order they need to be compiled.
   */
  private def getRelevantFiles(fileName: FileName): List[(FileName, List[Stat])] = {
    import scala.jdk.CollectionConverters._
    import ProjectCompiler.makeSubgraphOfDAG

    var g: DirectedAcyclicGraph[FileName, DefaultEdge] = null
    // Synchronize reads to the shared and mutable dependencyGraph
    this.synchronized {
      // Make a copy of the dependency graph
      val dependencies = dependencyGraph.getAncestors(fileName).asScala.toSet
      val relevantFiles = dependencies + fileName
      // Make a subgraph of the DAG containing only the relevant files
      g = makeSubgraphOfDAG(dependencyGraph, relevantFiles)
    }

    // Topologically sort the graph to get the order of the files
    g.iterator().asScala.map(fileName => (fileName, sources.get(fileName).get.stats)).toList
  }

  def makeProgramCompilerForFile(fileName: String): ProgramCompiler = {
    // Only compile the file in which the class is defined and its dependencies
    val filesInOrder = getRelevantFiles(fileName)
    val allFiles = addBuiltInKinds(filesInOrder)
    // Glue the files together and analyze the class
    val fileWithBuiltIns = glueFiles(allFiles)
    ProgramCompiler(fileWithBuiltIns)
  }

  def makeProgramCompiler[T](key: T, index: Map[T, FileName]): Option[ProgramCompiler] = {
    index
      .get(key)
      .map(makeProgramCompilerForFile)
  }

  def makeProgramCompilerForClass(className: String): Option[ProgramCompiler] = {
    makeProgramCompiler(className, classIndex)
  }

  def makeProgramCompilerForObject(objectName: String): Option[ProgramCompiler] = {
    makeProgramCompiler(objectName, objectIndex)
  }

  private def getProofCompiler(proof: ProofName): ProgramCompiler = {
    val (objectName, _) = proof
    makeProgramCompilerForObject(objectName) match {
      case Some(compiler) => compiler
      case None => throw new Error(s"Proof $proof not found")
    }
  }

  /**
   * Checks the given proof and does not return a counter example.
   * Do not try to call `counterExample` on the returned [[ProofResult]] as it will throw an error.
   * Also do not call `dispose()` on the returned [[ProofResult]], this has already been done for you.
   */
  def checkProof(proof: ProofName, maxTries: Int = 5, timeout: Int = 10000): ProofResult = {
    val compiler = getProofCompiler(proof)
    compiler.checkProof(proof, maxTries, timeout)
  }

  /**
   * Checks the given proof and returns a counter example if it is rejected.
   * Do not forget to call `dispose` on the returned [[ProofResult]]!
   */
  def checkProofForModel(proof: ProofName, maxTries: Int = 5, timeout: Int = 10000): ProofResult = {
    val compiler = getProofCompiler(proof)
    compiler.checkProofForModel(proof, maxTries, timeout)
  }

  def checkProofs(maxTries: Int = 5, timeout: Int = 10000): Map[ProofName, ProofResult] = {
    import scala.collection.parallel.CollectionConverters._

    // Compile independent components in parallel
    filesPerComponent.par.map {
      case filesInOrder => {
        // Glue all files of this component together
        ProgramCompiler(glueFiles(filesInOrder)).checkProofs(maxTries, timeout)
      }
    }.fold(Map())(_ ++ _)
  }

  /**
   * Runs all proofs and print the result to the console.
   */
  def printProofs(maxTries: Int = 5, timeout: Int = 10000): Unit = {
    import scala.collection.parallel.CollectionConverters._
    println("Running proofs...")
    objectIndex.keys.foreach {
      case objectName => {
        makeProgramCompilerForObject(objectName) match {
          case Some(compiler) =>
            compiler.newProofIndex.keys.par.foreach {
              case proof => {
                val proofName = s"- ${proof._1}.${proof._2}"
                val proofRes = compiler.checkProofForModel(proof, maxTries, timeout)
                // Synchronize the printing part to avoid the output of different proofs to be intermingled
                this.synchronized {
                  proofRes match {
                    case Proved(_) => println(Console.GREEN + s"$proofName [proved]")
                    case rej @ Rejected(_, _, _, _, _, _) => println(Console.RED + s"$proofName [rejected]" + s"\nCounterexample:\n$rej")
                    case Aborted(_) => println(Console.BLUE + s"$proofName [timed out]")
                  }
                }
                proofRes.dispose() // free the memory used by Z3
              }
            }
          case None =>
            throw new Error(s"Object $objectName not found")
        }
      }
    }
  }
}
