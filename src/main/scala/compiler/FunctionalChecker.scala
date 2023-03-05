package compiler

import compiler.irs.Asts
import Asts.*

object FunctionalChecker {

  def isPurelyFunctional(statement: Statement)(implicit analysisContext: AnalysisContext): Boolean = {
    isPurelyFunctionalImpl(statement)(Set.empty, analysisContext)
  }

  private def isPurelyFunctionalImpl(statement: Statement)(implicit funNotToCheck: Set[String], analysisContext: AnalysisContext): Boolean = {
    statement match
      case _: Asts.Literal => true
      case VariableRef(_) => true
      case Call(name, _) if funNotToCheck.contains(name) => true
      case Call(name, args) => {
        def bodyIsFullyFunctional = {
          analysisContext.functions.get(name).flatMap(_.optDef) match {
            case None => false // built-in function or function not found
            case Some(funDef) => isPurelyFunctionalImpl(funDef, funNotToCheck, analysisContext)
          }
        }
        args.forall(isPurelyFunctionalImpl) && bodyIsFullyFunctional
      }
      case Indexing(indexed, arg) =>
        isPurelyFunctionalImpl(indexed) && isPurelyFunctionalImpl(arg)
      case ArrayInit(_, size) =>
        isPurelyFunctionalImpl(size)
      case FilledArrayInit(arrayElems) =>
        arrayElems.forall(isPurelyFunctionalImpl)
      case StructInit(_, args) =>
        args.forall(isPurelyFunctionalImpl)
      case UnaryOp(_, operand) =>
        isPurelyFunctionalImpl(operand)
      case BinaryOp(lhs, _, rhs) =>
        isPurelyFunctionalImpl(lhs) && isPurelyFunctionalImpl(rhs)
      case Select(lhs, _) =>
        isPurelyFunctionalImpl(lhs)
      case Ternary(cond, thenBr, elseBr) =>
        isPurelyFunctionalImpl(cond) && isPurelyFunctionalImpl(thenBr) && isPurelyFunctionalImpl(elseBr)
      case Cast(expr, _) =>
        isPurelyFunctionalImpl(expr)
      case Sequence(stats, exprOpt) =>
        stats.forall(isPurelyFunctionalImpl) && exprOpt.exists(isPurelyFunctionalImpl)
      case Block(_) => false
      case LocalDef(_, _, rhs, isReassignable) =>
        !isReassignable && isPurelyFunctionalImpl(rhs)
      case VarAssig(_, _) => false
      case VarModif(_, _, _) => false
      case IfThenElse(_, _, _) => false
      case WhileLoop(_, _, _) => false
      case ForLoop(_, _, _, _, _) => false
      case ReturnStat(_) => false
      case PanicStat(_) => false
      case Assertion(_, _, _) => false
  }

  def isPurelyFunctional(funDef: FunDef)(implicit funNotToCheck: Set[String], analysisContext: AnalysisContext): Boolean = {
    isPurelyFunctionalImpl(funDef, funNotToCheck, analysisContext)
  }

  private def isPurelyFunctionalImpl(funDef: FunDef, funNotToCheck: Set[String], analysisContext: AnalysisContext): Boolean = {
    implicit val newFunNotToCheck: Set[String] = funNotToCheck + funDef.funName
    implicit val _analysisContext: AnalysisContext = analysisContext

    val stats = funDef.body.stats
    val initCond = stats.init.forall(stat => stat.isInstanceOf[LocalDef] && isPurelyFunctionalImpl(stat))

    def lastCond = stats.last match {
      case ReturnStat(Some(retVal)) if isPurelyFunctionalImpl(retVal) => true
      case _ => false
    }

    initCond && lastCond
  }

}
