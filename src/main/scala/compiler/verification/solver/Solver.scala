package compiler.verification.solver

import compiler.verification.solver.Solver
import smtlib.trees.Commands.Script
import smtlib.trees.Terms.Term

import scala.util.Try

trait Solver {
  
  def initialize(): Unit

  /**
   * @param smtScript SMT script to be checked for satisfiability
   * @param timeoutSec timeout to give to the solver, in seconds
   * @param comments comments to be added to the files given to the solver
   * @param idx index of the path corresponding to this formula
   */
  def check(smtScript: Script, timeoutSec: Int, comments: String, idx: Int): Solver.Result

}

object Solver {

  sealed trait Result
  final case class Sat(varsAssigDescr: String) extends Result
  case object Unsat extends Result
  final case class Timeout(timeoutSec: Int) extends Result
  final case class Error(msg: String) extends Result

}
