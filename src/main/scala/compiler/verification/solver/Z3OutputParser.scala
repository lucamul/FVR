package compiler.verification.solver

import lang.Operator.{ClosingParenthesis, Minus, OpeningParenthesis}
import smtlib.trees.CommandsResponses.Success

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.{Failure, Try}
import scala.collection.mutable

object Z3OutputParser {

  /**
   * @param lines z3 response, line by line
   * @return an assignment of variables obtained by parsing z3 response, or a failure if parsing failed
   */
  def parse(lines: List[String]): Try[Map[String, String]] = {

    def fail: Try[Map[String, String]] = {
      Failure(new Exception(s"unable to parse z3 response"))
    }

    def parseVariableLine(line: String): Option[String] = {
      val (toParse, rem) = {
        line
          .trim
          .replace("(define-fun ", "")
          .span(!_.isWhitespace)
      }
      if rem.trim.startsWith("()") then Some(toParse) else None
    }

    def parseValueLine(line: String): String = {
      line.trim.init  // drop terminal parenthesis
    }

    if (lines.size >= 2 && lines.head.trim == "(" && lines.last.trim == ")") {
      val linesIter = lines.iterator
      val assig = mutable.Map.empty[String, String]
      linesIter.next()  // drop first line
      while (linesIter.hasNext){
        val currLine = linesIter.next()
        if (!linesIter.hasNext){
          return scala.util.Success(assig.toMap)
        }
        val variableOpt = parseVariableLine(currLine)
        // get value even if variableOpt is None, to move the iterator
        val value = {
          if (linesIter.hasNext){
            parseValueLine(linesIter.next())
          } else {
            return fail
          }
        }
        variableOpt match {
          case Some(variable) =>
            assig.put(variable, value)
          case None => ()
        }
      }
      scala.util.Success(assig.toMap)
    } else {
      fail
    }
  }

}
