package compiler.verification.solver

import compiler.verification.solver.Solver
import compiler.verification.solver.Solver.*
import smtlib.trees.Commands.{CheckSat, GetModel, Script}
import smtlib.trees.{Commands, Terms}
import compiler.FileExtensions

import java.io.{BufferedReader, FileWriter, IOException, InputStreamReader}
import java.nio.file.Files
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec
import scala.util.{Failure, Success, Try, Using}

/**
 * Interface to the Z3 command-line solver
 * @param outputDir the directory in which z3 input files will be written
 */
final class Z3Solver(outputDir: java.nio.file.Path) extends Solver {
  private val z3ExecName = "z3"
  private val filenamePrefix = "z3input"

  private def nextFilepath(idx: Int) = {
    outputDir.resolve(s"${filenamePrefix}_$idx.${FileExtensions.smt}")
  }


  override def initialize(): Unit = {
    val dir = outputDir.toFile
    if (dir.exists()) {
      clearZ3InputFilesInDir()
    } else {
      dir.mkdir()
    }
  }

  override def check(smtScript: Commands.Script, timeoutSec: Int, comments: String, idx: Int): Result = {
    val filepath = nextFilepath(idx)
    writeFile(smtScript, filepath, comments).map { _ =>
      val timeoutOption = s"-T:$timeoutSec"
      runCommand(List(z3ExecName, timeoutOption, filepath.toString), timeoutSec)
    }.recover(exc => Error("Internal error: " ++ exc.getMessage)).get
  }

  private def clearZ3InputFilesInDir(): Unit = {
    for file <- outputDir.toFile.listFiles() do {
      if (file.getName.startsWith(filenamePrefix)){
        file.delete()
      }
    }
  }

  private def writeFile(script: Script, tmpFilePath: java.nio.file.Path, comments: String): Try[Unit] = {
    val file = tmpFilePath.toFile
    file.createNewFile()
    Using(new FileWriter(file)) { writer =>
      writeComments(comments, writer)
      val printer = smtlib.printer.RecursivePrinter
      printer.printScript(script, writer)
      printer.printCommand(CheckSat(), writer)
      printer.printCommand(GetModel(), writer)
    }
  }

  private def writeComments(comments: String, writer: FileWriter): Unit = {
    for line <- io.Source.fromString(comments).getLines() do {
      writer.write(s"; $line\n")
    }
  }

  private def runCommand(cmd: List[String], timeoutSec: Int): Result = {

    // adapted from https://stackoverflow.com/questions/5711084/java-runtime-getruntime-getting-output-from-executing-a-command-line-program

    val runtime = Runtime.getRuntime
    val command = cmd.mkString(" ")
    val process = runtime.exec(command)

    val tryRes = {

      Using(new BufferedReader(new InputStreamReader(process.getInputStream))) { reader =>

        // read all the lines until none is left
        @tailrec def read(prevLinesRev: List[String]): List[String] = {
          val line = reader.readLine()
          if (line != null) {
            read(line :: prevLinesRev)
          } else {
            prevLinesRev.reverse
          }
        }

        // read first line, then if sat read the other lines to try to parse an assignment
        reader.readLine() match {
          case "sat" =>
            Z3OutputParser.parse(read(Nil)) match {
              case Failure(exception) => Sat(" error: " ++ exception.getMessage)  // sat but parsing failed
              case Success(assigMap) =>
                val prefix = if assigMap.isEmpty then "" else "Could not be verified e.g. for: "
                Sat(    // sat and assignment to display
                  assigMap
                    .filter((name, _) => isOriginalVarName(name))   // drop variables obtained by desugaring/renaming
                    .map((name, value) => s"$name == $value")
                    .mkString(prefix, " && ", "")
                )
            }
          case "unsat" => Unsat
          case "timeout" => Timeout(timeoutSec)
          case s => Error(s)
        }

      }
    }

    tryRes match {
      case Failure(exception) => Error("Internal error: " ++ exception.getMessage)
      case Success(result) => result
    }
  }

  /**
   * @return true if `name` is a variable of the original program, false if it was obtained by renaming/desugaring
   */
  private def isOriginalVarName(name: String): Boolean = {
    require(name.nonEmpty)
    name.head.isLetter && name.tail.forall(char => char.isLetterOrDigit || char == '_')
  }

}
