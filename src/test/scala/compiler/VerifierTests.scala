package compiler

import compiler.io.SourceFile
import compiler.verification.solver.Solver
import org.junit.Assert.assertEquals
import org.junit.{After, Before, Test}
import util.IO

import java.io.File
import java.nio.file.Paths

class VerifierTests {
  private val outputDirPath = "verifierTestsTmp"
  private val resDirPath = "src/test/res/verif"

  @After def deleteTmpDir(): Unit = {
    IO.deleteRecursively(new File(outputDirPath))
  }

  @Test def answer42Test(): Unit = runVerifTest("answer42", true)
  @Test def reluTest(): Unit = runVerifTest("relu", true)
  @Test def reluFailTest(): Unit = runVerifTest("reluFail", false)
  @Test def squareTest(): Unit = runVerifTest("square", true)
  @Test def verifLoopTest(): Unit = runVerifTest("verifloop", true)
  @Test def loopFailTest1(): Unit = runVerifTest("loopFail1", false)
  @Test def loopFailTest2(): Unit = runVerifTest("loopFail2", false)
  @Test def verifValTest(): Unit = runVerifTest("verifval", true)
  @Test def maxTest(): Unit = runVerifTest("max", true)
  @Test def maxFailTest(): Unit = runVerifTest("maxFail", false)
  @Test def fibonacci1Test(): Unit = runVerifTest("fibonacci1", true)
  @Test def fibonacci2Test(): Unit = runVerifTest("fibonacci2", true)
  @Test def fibonacci3Test(): Unit = runVerifTest("fibonacci3", true)
  @Test def fibonacci4Test(): Unit = runVerifTest("fibonacci4", true)
  @Test def fibonacci5Test(): Unit = runVerifTest("fibonacci5", true)
  @Test def fibonacciFailTest(): Unit = runVerifTest("fibonacciFail", false)
  @Test def prodTest(): Unit = runVerifTest("prod", true)
  @Test def prodFailTest(): Unit = runVerifTest("prodFail", false)
  @Test def boolsTest(): Unit = runVerifTest("bools", true)
  @Test def euclidWeakTest(): Unit = runVerifTest("euclidWeak", true)
  @Test def euclidWeakFailTest(): Unit = runVerifTest("euclidWeakFail", false)
  @Test def forLoopTest(): Unit = runVerifTest("forloop", true)
  @Test def forLoopFailTest(): Unit = runVerifTest("forLoopFail", false)
  @Test def minEquivTest(): Unit = runVerifTest("minequiv", true)
  @Test def minEquivFail1Test(): Unit = runVerifTest("minequivFail1", false)
  @Test def minEquivFail2Test(): Unit = runVerifTest("minequivFail2", false)
  @Test def minEquivFail3Test(): Unit = runVerifTest("minequivFail3", false)
  @Test def uncheckedTest(): Unit = runVerifTest("unchecked", true)
  @Test def uncheckedFailTest(): Unit = runVerifTest("uncheckedFail", false)
  @Test def avgTest(): Unit = runVerifTest("avg", true)
  @Test def avgFailTest(): Unit = runVerifTest("avgFail", false)
  @Test def arraySzTest(): Unit = runVerifTest("arrsz", true)
  @Test def arraySzFail1Test(): Unit = runVerifTest("arrszFail1", false)
  @Test def arraySzFail2Test(): Unit = runVerifTest("arrszFail2", false)
  @Test def arraySzFail3Test(): Unit = runVerifTest("arrszFail3", false)
  @Test def arraySzFail4Test(): Unit = runVerifTest("arrszFail4", false)
  @Test def arraySzFail5Test(): Unit = runVerifTest("arrszFail5", false)
  @Test def arraySzFail6Test(): Unit = runVerifTest("arrszFail6", false)
  @Test def arraySzFail7Test(): Unit = runVerifTest("arrszFail7", false)
  @Test def arraySzFail8Test(): Unit = runVerifTest("arrszFail8", false)
  @Test def arraySzFail9Test(): Unit = runVerifTest("arrszFail9", false)
  @Test def arraySzFail10Test(): Unit = runVerifTest("arrszFail10", false)
  @Test def arraySzFail11Test(): Unit = runVerifTest("arrszFail11", false)
  @Test def arraySzFail12Test(): Unit = runVerifTest("arrszFail12", false)
  @Test def elem10Test(): Unit = runVerifTest("elem10", true)
  @Test def elem10Fail1Test(): Unit = runVerifTest("elem10Fail1", false)
  @Test def elem10Fail2Test(): Unit = runVerifTest("elem10Fail2", false)
  @Test def elem10Fail3Test(): Unit = runVerifTest("elem10Fail3", false)
  @Test def concatTest(): Unit = runVerifTest("concat", true)
  @Test def concatFail1Test(): Unit = runVerifTest("concatFail1", false)
  @Test def concatFail2Test(): Unit = runVerifTest("concatFail2", false)
  @Test def concatFail3Test(): Unit = runVerifTest("concatFail3", false)
  @Test def arrPosTest(): Unit = runVerifTest("arrPos", true)
  @Test def arrPosFailTest1(): Unit = runVerifTest("arrPosFail1", false)
  @Test def arrPosFailTest2(): Unit = runVerifTest("arrPosFail2", false)
  @Test def arrPosFailTest3(): Unit = runVerifTest("arrPosFail3", false)
  @Test def arrPosFailTest4(): Unit = runVerifTest("arrPosFail4", false)
  @Test def arrPosFailTest5(): Unit = runVerifTest("arrPosFail5", false)
  @Test def arrPosFailTest6(): Unit = runVerifTest("arrPosFail6", false)
  @Test def refEqTest(): Unit = runVerifTest("refeq", true)
  @Test def refEqFail1Test(): Unit = runVerifTest("refeqFail1", false)
  @Test def refEqFail2Test(): Unit = runVerifTest("refeqFail2", false)
  @Test def aliasingFail1Test(): Unit = runVerifTest("aliasingFail1", false)
  @Test def aliasingFail2Test(): Unit = runVerifTest("aliasingFail2", false)
  @Test def arrayAccessFail(): Unit = runVerifTest("arraccessFail", false)


  private def runVerifTest(filename: String, expectedRes: Boolean, timeoutSec: Int = 2): Unit = {
    val pipeline = TasksPipelines.verifier(Paths.get(outputDirPath), timeoutSec, logger = _ => ())
    val path = s"$resDirPath/$filename.${FileExtensions.rattlesnake}"
    val actualRes = pipeline.apply(List(SourceFile(path))).allPassed
    assertEquals(expectedRes, actualRes)
  }

}
