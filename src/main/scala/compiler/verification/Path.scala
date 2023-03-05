package compiler.verification

import compiler.irs.Asts
import compiler.irs.Asts.*
import compiler.prettyprinter.PrettyPrinter
import compiler.verification.Path.{PathElement, isControlFlowStat}
import lang.Types.PrimitiveType.BoolType
import compiler.Replacer.replaceInExpr

import scala.collection.mutable.ListBuffer

/**
 * Program path
 * @param pathElems statements of the path (before the postcondition)
 * @param formulaToProve postcondition, must be true at the end of the execution of the statements
 * @param descr description of the path
 */
final case class Path(pathElems: List[PathElement], formulaToProve: Expr, descr: String) {
  require(formulaToProve.getType == BoolType)

  /**
   * Integrity check: fails the program if an expression (in a statement or in the postcondition) has not been assigned a type
   */
  def assertAllTypesAreSet(): Unit = {
    for pathElem <- pathElems do {
      pathElem.assertAllTypesAreSet()
    }
    formulaToProve.assertAllTypesAreSet()
  }

  override def toString: String = {
    val prettyPrinter = new PrettyPrinter()
    val statsLines = pathElems.map(prettyPrinter.apply(_))
    val line = "-".repeat(if statsLines.isEmpty then 20 else statsLines.maxBy(_.length).length)
    val formulaToProveLine = " ==> " ++ prettyPrinter.apply(formulaToProve) ++ s"  [$descr]"
    (statsLines ++ List(line, formulaToProveLine)).mkString("\n")
  }

}

object Path {

  /**
   * Statement that can be added to a path
   */
  type PathElement = Assertion | LocalDef | VarAssig

  /**
   * Builder for a path
   * 
   * `builtWith` has no side effects and a builder can be used to create more than one path
   * (build a path, then add new statements, and build a longer path)
   */
  final class Builder(varsCtx: VarsCtx = new VarsCtx()) {
    private val stats = ListBuffer.empty[PathElement]

    def addPathElem(elem: PathElement): Builder = {
      stats.addOne(removeVars(elem))
      this
    }

    /**
     * @return a new builder with all the statements contained in this
     */
    def copied: Builder = {
      val copy = new Builder(varsCtx.copied)
      copy.stats.addAll(stats)
      copy
    }

    /**
     * @return a new path with all the statements in this builder and the provided formula and description
     */
    def builtWith(formulaToProve: Expr, descr: String): Path = {
      require(formulaToProve.getType == BoolType)
      val formulaAfterRenaming = replaceInExpr(formulaToProve, varsCtx.currentRenameMapSnapshot)
      Path(stats.toList, formulaAfterRenaming, descr)
    }
    
    /*
     * The following `removeVars` functions make the path functional by replacing the assignments to variables 
     * with definitions of new values:
     *
     *  var x = 0;    ---->    val x = 0;
     *  x = 42;                val x%1 = 42;
     */

    private def removeVars(stats: List[Statement]): List[Statement] = {
      stats.map(removeVars)
    }

    private def removeVars(pathElement: PathElement): PathElement = {
      pathElement match {
        case LocalDef(localName, optType, rhs, _) =>
          val newRhs = replaceInExpr(rhs, varsCtx.currRenameMapView)
          val newName = varsCtx.newNameFor(localName)
          LocalDef(newName, optType, newRhs, isReassignable = false)
        case VarAssig(VariableRef(name), rhs) =>
          val newRhs = replaceInExpr(rhs, varsCtx.currRenameMapView)
          val newName = varsCtx.newNameFor(name)
          LocalDef(newName, Some(rhs.getType), newRhs, isReassignable = false)
        case VarAssig(lhs, rhs) =>
          val renMap = varsCtx.currRenameMapView
          val newLhs = replaceInExpr(lhs, renMap)
          val newRhs = replaceInExpr(rhs, renMap)
          VarAssig(newLhs, newRhs)
        case assertion@Assertion(formulaExpr, _descr, isAssumed) =>
          Assertion(
            replaceInExpr(formulaExpr, varsCtx.currRenameMapView),
            _descr,
            isAssumed
          ).setPositionSp(assertion.getPosition)
      }
    }

    private def removeVars(expr: Expr): Expr = {
      val res = expr match {
        case literal: Asts.Literal =>
          literal
        case VariableRef(name) =>
          VariableRef(varsCtx.nameFor(name))
        case Call(callee, args) =>
          Call(callee, args.map(removeVars))
        case Indexing(indexed, arg) =>
          Indexing(removeVars(indexed), removeVars(arg))
        case ArrayInit(elemType, size) =>
          ArrayInit(elemType, removeVars(size))
        case FilledArrayInit(arrayElems) =>
          FilledArrayInit(arrayElems.map(removeVars))
        case StructInit(structName, args) =>
          StructInit(structName, args.map(removeVars))
        case UnaryOp(operator, operand) =>
          UnaryOp(operator, removeVars(operand))
        case BinaryOp(lhs, operator, rhs) =>
          BinaryOp(removeVars(lhs), operator, removeVars(rhs))
        case Select(lhs, selected) =>
          Select(removeVars(lhs), selected)
        case Ternary(cond, thenBr, elseBr) =>
          Ternary(removeVars(cond), removeVars(thenBr), removeVars(elseBr))
        case Cast(expr, tpe) =>
          Cast(removeVars(expr), tpe)
        case Sequence(stats, exprOpt) =>
          Sequence(stats.map(removeVars), exprOpt.map(removeVars))
      }
      res.setType(expr.getType)
    }

    private def removeVars(statement: Statement): Statement = {
      statement match
        case expr: Expr =>
          removeVars(expr)
        case Block(stats) =>
          Block(removeVars(stats))
        case pathElement: PathElement =>
          removeVars(pathElement)
        case PanicStat(msg) =>
          PanicStat(removeVars(msg))
        case _: (VarModif | IfThenElse | WhileLoop | ForLoop | ReturnStat) =>
          assert(false)
    }

  }

  private def isControlFlowStat(statement: Statement): Boolean = {
    statement match

      case _: IfThenElse => true
      case _: WhileLoop => true
      case _: ForLoop => true
      case _: Block => true

      case _: Expr => false
      case _: LocalDef => false
      case _: Asts.Assignment => false
      case _: ReturnStat => false
      case _: PanicStat => false
      case _: Assertion => false
  }

}
