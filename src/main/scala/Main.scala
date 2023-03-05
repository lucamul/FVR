import compiler.Errors.{ErrorReporter, errorsExitCode}
import compiler.desugarer.Desugarer
import compiler.io.SourceFile
import compiler.parser.LL1Iterator
import compiler.{FileExtensions, SourceCodeProvider, TasksPipelines}
import lang.Types.ArrayType
import lang.Types.PrimitiveType.StringType
import org.objectweb.asm.Opcodes.{V11, V17, V1_8}

import java.nio.file.{Files, InvalidPathException, Path, Paths}
import scala.annotation.tailrec
import scala.collection.mutable

object Main {

  private val cmdLineExitCode = -22

  private val defaultVerifTimeout = 2

  private val java8Tag = "java8"
  private val java11Tag = "java11"
  private val java17Tag = "java17"
  private val knownJavaVersions = Map(
    java8Tag -> V1_8,
    java11Tag -> V11,
    java17Tag -> V17
  )

  private type MutArgsMap = mutable.Map[String, Option[String]]

  def main(args: Array[String]): Unit = {
    val cmdLine = args.mkString(" ")
    try {
      val (action, pathStrs) = parseCmdLine(splitAtSpacesExceptBetweenBrackets(cmdLine))
      if (pathStrs.exists(!_.endsWith(FileExtensions.rattlesnake))){
        error(s"all sources must be .${FileExtensions.rattlesnake} files")
      }
      val sourceFiles = pathStrs.map(SourceFile.apply)
      action.run(sourceFiles)
    } catch {
      case e: InvalidPathException => error(e.getMessage)
    }
  }

  private def splitAtSpacesExceptBetweenBrackets(cmdLine: String): List[String] = {

    val charsAndDepths = cmdLine.foldLeft(List((0.toChar, 0))){ (reversedCharsAndDepths, currChar) =>
      val currDepth = reversedCharsAndDepths.head._2
      currChar match {
        case '[' if currDepth > 0 => error("nested lists are not supported")
        case '[' => (currChar, currDepth + 1) :: reversedCharsAndDepths
        case ']' if currDepth == 0 => error("unexpected ']'")
        case ']' => (currChar, currDepth - 1) :: reversedCharsAndDepths
        case _ => (currChar, currDepth) :: reversedCharsAndDepths
      }
    }.reverse.tail

    @tailrec def split(wordsReversed: List[String], currWordReversed: List[Char], charsAndDepth: List[(Char, Int)]): List[String] = {
      charsAndDepth match {
        case Nil =>
          (currWordReversed.reverse.mkString :: wordsReversed).reverse
        case (currChar, 0) :: tail if currChar.isWhitespace =>
          split(currWordReversed.reverse.mkString :: wordsReversed, Nil, tail)
        case (currChar, _) :: tail =>
          split(wordsReversed, currChar :: currWordReversed, tail)
      }
    }

    split(Nil, Nil, charsAndDepths)
  }

  private def parseCmdLine(cmdLine: List[String]): (Action, List[String]) = {
    cmdLine match {
      case Nil => error("empty command")
      case cmd :: tail => {
        if (cmd == "help"){
          displayHelp()
          System.exit(0)
          throw new AssertionError() // should never happen because exit occurred before
        }
        val (args, files) = tail.span(_.startsWith("-"))
        if (files.isEmpty){
          error("no input files")
        }
        val argsMap = parseArgs(Nil, args.map(_.substring(1)))
        cmd match {
          case "run" => (Run(argsMap), files)
          case "compile" => (Compile(argsMap), files)
          case "asm" => (Asm(argsMap), files)
          case "format" => (Format(argsMap), files)
          case "typecheck" => (TypeCheck(argsMap), files)
          case "desugar" => (Desugar(argsMap), files)
          case "verify" => (Verify(argsMap), files)
          case _ => error(s"unknown command: $cmd")
        }
      }
    }
  }

  /**
   * Parse the arguments of a command
   * @param alrParsed already parsed
   * @param rem remaining words to parse
   * @return a map arg -> value
   */
  @tailrec private def parseArgs(alrParsed: List[(String, Option[String])], rem: List[String]): MutArgsMap = {
    rem match {
      case Nil => mutable.Map.from(alrParsed)
      case head :: tail =>
        head.split("=", 2).toList match {
          case noValArg :: Nil => parseArgs((noValArg, None) :: alrParsed, tail)
          case arg :: value :: Nil => parseArgs((arg, Some(value)) :: alrParsed, tail)
          case _ => assert(false)
        }
    }
  }

  private def createDefaultBytecodeOutputName(sources: List[SourceCodeProvider]): String = {
    Path.of(sources.head.name).getFileName.toString.takeWhile(_ != '.').withHeadUppercase + "_core"
  }

  private def parseJavaVersion(str: String): Int = {
    knownJavaVersions.getOrElse(str, error(s"unknown java version, known are only ${knownJavaVersions.mkString(", ")}"))
  }

  private def getValuedArg(argName: String, argsMap: MutArgsMap, optDefault: Option[String] = None): String = {
    argsMap.remove(argName).getOrElse(optDefault).getOrElse(error(s"missing required argument: $argName"))
  }

  private def getUnvalArg(argName: String, argsMap: MutArgsMap): Boolean = {
    argsMap.remove(argName) match {
      case None => false
      case Some(None) => true
      case Some(_) => error(s"argument $argName takes no value")
    }
  }

  private def getOutputNameArg(sources: List[SourceCodeProvider], argsMap: MutArgsMap, defaultOutputName: String): String = {
    getValuedArg("out-file", argsMap, Some(defaultOutputName))
  }

  private def getOutDirArg(argsMap: MutArgsMap): Path = {
    Paths.get(getValuedArg("out-dir", argsMap, Some(".")))
  }

  private def getJavaVersionArg(argsMap: MutArgsMap): Int = {
    parseJavaVersion(getValuedArg("java-version", argsMap, Some(java8Tag)))
  }

  private def getIndentGranularityArg(argsMap: MutArgsMap): Int = {
    val argStr = getValuedArg("indent", argsMap, Some("2"))
    val indent = argStr.toIntOption.getOrElse(error(s"could not convert $argStr to an integer"))
    if (indent <= 0){
      error("indent must be positive")
    }
    indent
  }

  private def getTimeoutArg(argsMap: MutArgsMap): Int = {
    val argStr = getValuedArg("timeout", argsMap, Some(defaultVerifTimeout.toString))
    val timeout = argStr.toIntOption.getOrElse(error(s"could not convert $argStr to an integer"))
    if (timeout <= 0){
      error("timeout must be positive")
    }
    timeout
  }

  private def getPrintAllParenthesesArg(argsMap: MutArgsMap): Boolean = {
    getUnvalArg("all-parenth", argsMap)
  }

  private def getDesugarerModeArg(argsMap: MutArgsMap): Desugarer.Mode = {
    if getUnvalArg("compile-like", argsMap) then Desugarer.Mode.Compile
    else Desugarer.Mode.Verify
  }

  private def getRenameVarsArg(argsMap: MutArgsMap): Boolean = {
    getUnvalArg("rename-vars", argsMap)
  }

  private def getProgramArgsArg(argsMap: MutArgsMap): Array[String] = {
    val emptyArrStr = "[]"
    val arrayStr = getValuedArg("args", argsMap, Some(emptyArrStr))
    if (!(arrayStr.startsWith("[") && arrayStr.endsWith("]"))){
      error("program arguments must be given as a list (surrounded by brackets and separated by whitespaces)")
    }
    if arrayStr == emptyArrStr then Array.empty else arrayStr.tail.init.split(' ')
  }

  // Actions, i.e. description of commands to the cmdline program -----------------------------------------------

  private trait Action {
    def run(sources: List[SourceCodeProvider]): Unit
  }

  /**
   * Run command (compile and run)
   */
  private case class Run(argsMap: MutArgsMap) extends Action {
    override def run(sources: List[SourceCodeProvider]): Unit = {
      val outputName = getOutputNameArg(sources, argsMap, createDefaultBytecodeOutputName(sources))
      val compiler = TasksPipelines.compiler(
        getOutDirArg(argsMap),
        getJavaVersionArg(argsMap),
        outputName
      )
      val programArgs = getProgramArgsArg(argsMap)
      reportUnknownArgsIfAny(argsMap)
      val writtenFilesPaths = compiler.apply(sources)
      val classes = {
        for path <- writtenFilesPaths yield {
          val bytes = Files.readAllBytes(path)
          val className = path.getFileName.toString.takeWhile(_ != '.')
          Loader.load(className, bytes)
        }
      }
      val coreClass = classes.find(_.getName == outputName).get
      val mainMethod = coreClass.getDeclaredMethods.find(_.getName == "main").getOrElse(error("no main function"))
      if (!mainMethod.getParameterTypes.sameElements(Array(classOf[Array[String]]))){
        error("no function with signature 'main(arr String)'")
      }
      mainMethod.invoke(null, programArgs)
    }
  }

  /**
   * Compile command
   */
  private case class Compile(argsMap: MutArgsMap) extends Action {
    override def run(sources: List[SourceCodeProvider]): Unit = {
      val compiler = TasksPipelines.compiler(
        getOutDirArg(argsMap),
        getJavaVersionArg(argsMap),
        getOutputNameArg(sources, argsMap, createDefaultBytecodeOutputName(sources))
      )
      reportUnknownArgsIfAny(argsMap)
      val cnt = compiler.apply(sources).size
      succeed(s"wrote $cnt file(s)")
    }
  }

  /**
   * ASM command (generate JVM bytecode as a text file)
   */
  private case class Asm(argsMap: MutArgsMap) extends Action {
    override def run(sources: List[SourceCodeProvider]): Unit = {
      val bytecodeWriter = TasksPipelines.bytecodeWriter(
        getOutDirArg(argsMap),
        getJavaVersionArg(argsMap),
        getOutputNameArg(sources, argsMap, createDefaultBytecodeOutputName(sources))
      )
      reportUnknownArgsIfAny(argsMap)
      val cnt = bytecodeWriter.apply(sources).size
      succeed(s"wrote $cnt file(s)")
    }
  }

  /**
   * Format command (format a file)
   */
  private case class Format(argsMap: MutArgsMap) extends Action {
    override def run(sources: List[SourceCodeProvider]): Unit = {
      if (sources.size != 1) {
        error("format command requires exactly 1 input file")
      }
      val formatter = TasksPipelines.formatter(
        getOutDirArg(argsMap),
        getOutputNameArg(sources, argsMap, Path.of(sources.head.name).getFileName.toString),
        getIndentGranularityArg(argsMap),
        quest => yesNoQuestion(quest),
        getPrintAllParenthesesArg(argsMap)
      )
      reportUnknownArgsIfAny(argsMap)
      formatter.apply(sources.head)
      succeed()
    }
  }

  /**
   * Typecheck command (typecheck a file)
   */
  private case class TypeCheck(argsMap: MutArgsMap) extends Action {
    override def run(sources: List[SourceCodeProvider]): Unit = {
      reportUnknownArgsIfAny(argsMap)
      TasksPipelines.typeChecker.apply(sources)
      succeed()
    }
  }

  /**
   * Desugar command (show the file after desugaring)
   */
  private case class Desugar(argsMap: MutArgsMap) extends Action {
    override def run(sources: List[SourceCodeProvider]): Unit = {
      if (sources.size != 1) {
        error("desugar command requires exactly 1 input file")
      }
      val desugarer = TasksPipelines.desugarer(
        getOutDirArg(argsMap),
        getOutputNameArg(sources, argsMap, Path.of(sources.head.name).getFileName.toString),
        getDesugarerModeArg(argsMap),
        getIndentGranularityArg(argsMap),
        quest => yesNoQuestion(quest),
        getPrintAllParenthesesArg(argsMap),
        getRenameVarsArg(argsMap)
      )
      reportUnknownArgsIfAny(argsMap)
      desugarer.apply(sources.head)
      succeed()
    }
  }

  private case class Verify(argsMap: MutArgsMap) extends Action {
    override def run(sources: List[SourceCodeProvider]): Unit = {
      val verifier = TasksPipelines.verifier(
        getOutDirArg(argsMap),
        getTimeoutArg(argsMap),
        logger = println
      )
      reportUnknownArgsIfAny(argsMap)
      val score = verifier.apply(sources)
      println
      if (score.allPassed) {
        println(s"Program verification succeeded ($score)")
      } else {
        println(s"Program verification FAILED ($score)")
      }
    }
  }

  private def error(msg: String): Nothing = {
    System.err.println(msg)
    System.exit(cmdLineExitCode)

    // should never happen
    throw new AssertionError()
  }

  private def reportUnknownArgsIfAny(argsMap: MutArgsMap): Unit = {
    if (argsMap.nonEmpty){
      error(s"unknown argument(s): ${argsMap.keys.mkString(", ")}")
    }
  }

  private def succeed(msg: String = ""): Unit = {
    if (msg.nonEmpty){
      println(msg)
    }
    println("task succeeded")
  }

  private def displayHelp(): Unit = {
    println(
      s"""
        |Command: <cmd> [<arg>*] <file>*
        |   e.g. run -out-dir=output examples/concat.rsn
        |
        |run: compile and run the program
        | args: -out-dir=...: required, directory where to write the output file
        |       -out-file=...: optional, output file name (by default <input file name>_core
        |       -java-version=...: optional, can be '$java8Tag', '$java11Tag' or '$java17Tag' (default is '$java8Tag')
        |       -args=[...]: optional, arguments to be passed to the executed program (e.g. -args=[foo bar baz])
        |compile: compile the program
        | args: -out-dir=...: required, directory where to write the output file
        |       -out-file=...: optional, output file name (by default <input file name>_core
        |       -java-version=...: optional, can be '$java8Tag', '$java11Tag' or '$java17Tag' (default is '$java8Tag')
        |asm: write JVM bytecode instructions to a text file
        | args: -out-dir=...: required, directory where to write the output file
        |       -out-file=...: optional, output file name (by default <input file name>_core
        |       -java-version=...: optional, can be '$java8Tag', '$java11Tag' or '$java17Tag' (default is '$java8Tag')
        |format: reformat file
        | args: -out-dir=...: required, directory where to write the output file
        |       -out-file=...: optional, output file name (by default same as input)
        |       -indent=...: optional, indent granularity (2 by default)
        |       -all-parenth: flag indicating that all parentheses should be displayed in expressions,
        |                     regardless of the priority of operations (takes no value)
        |typecheck: parse and typecheck the program
        |desugar: show the file after desugaring
        | args: -out-dir=...: required, directory where to write the output file
        |       -out-file=...: optional, output file name (by default same as input)
        |       -compile-like: flag indicating that desugaring must be done in the same way as before compilation
        |                      (by default it is done like before verification)
        |       -indent=...: optional, indent granularity (2 by default)
        |       -all-parenth: flag indicating that all parentheses should be displayed in expressions,
        |                     regardless of the priority of operations (takes no value)
        |       -rename-vars: flag indicating that renaming should be performed, i.e. they should have identifiers that
        |                     are unique across the program
        |verify: run formal verification on the input files
        | args: -out-dir=...: required, directory where to write the formula files (directory is created or cleared if
        |                     it already exists)
        |       -timeout=...: optional, timeout in seconds of each query to the solver (2 seconds by default)
        |help: displays help (this)
        |""".stripMargin)
  }

  extension (str: String) private def withHeadUppercase: String = {
    if str.isEmpty then str
    else str.head.toUpper +: str.tail
  }

  /** Class loader to load generated .class files when executing the `run` command */
  private object Loader extends ClassLoader(Thread.currentThread().getContextClassLoader) {
    def load(name: String, bytes: Array[Byte]): Class[_] = {
      super.defineClass(name, bytes, 0, bytes.length)
    }
  }

  @tailrec private def yesNoQuestion(prompt: String): Boolean = {
    println(prompt)
    val input = scala.io.StdIn.readLine()
    val lowerCaseInput = input.toLowerCase
    if (Set("y", "yes").contains(lowerCaseInput)) {
      true
    } else if (Set("n", "no").contains(lowerCaseInput)){
      false
    } else {
      println("expected 'yes' or 'no'")
      yesNoQuestion(prompt)
    }
  }

}
