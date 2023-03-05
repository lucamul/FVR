package compiler.renamer

import compiler.{AnalysisContext, CompilerStep}
import compiler.irs.Asts.*

/**
 * Intended to happen after desugaring
 */
final class Renamer() extends CompilerStep[(List[Source], AnalysisContext), (List[Source], AnalysisContext)] {

  override def apply(input: (List[Source], AnalysisContext)): (List[Source], AnalysisContext) = {
    val (sources, analysisCtx) = input
    val globalVarsCtx = new GlobalVarsCtx()
    val renamedSources = sources.map(rename(_, globalVarsCtx))
    (renamedSources, analysisCtx)
  }

  private def rename(src: Source, globalVarsCtx: GlobalVarsCtx): Source = {
    val Source(defs) = src
    Source(defs.map(rename(_, globalVarsCtx)))
  }

  private def rename(df: TopLevelDef, globalVarsCtx: GlobalVarsCtx): TopLevelDef = {
    df match
      case FunDef(funName, params, optRetType, body, precond, postcond, verifIgnore) =>
        assert(precond.isEmpty)
        assert(postcond.isEmpty)
        val ctx = new LocalVarsCtx(globalVarsCtx)
        val newParams = params.map(rename(_, ctx))
        val newBody = rename(body, ctx)
        FunDef(funName, newParams, optRetType, newBody, Nil, Nil, verifIgnore)
      case StructDef(structName, fields) =>
        val ctx = new LocalVarsCtx(globalVarsCtx)
        StructDef(structName, fields.map(rename(_, ctx)))
  }

  private def rename(expr: Expr, ctx: LocalVarsCtx): Expr = {

    def ren(expr: Expr): Expr = rename(expr, ctx)

    val res: Expr = expr match {
      case literal: Literal => literal
      case Call(callee, args) =>
        Call(callee, args.map(ren))
      case Indexing(indexed, arg) =>
        Indexing(ren(indexed), ren(arg))
      case ArrayInit(elemType, size) =>
        ArrayInit(elemType, ren(size))
      case FilledArrayInit(arrayElems) =>
        FilledArrayInit(arrayElems.map(ren))
      case StructInit(structName, args) =>
        StructInit(structName, args.map(ren))
      case UnaryOp(operator, operand) =>
        UnaryOp(operator, ren(operand))
      case BinaryOp(lhs, operator, rhs) =>
        BinaryOp(ren(lhs), operator, ren(rhs))
      case Select(lhs, selected) =>
        Select(ren(lhs), selected)
      case Ternary(cond, thenBr, elseBr) =>
        Ternary(ren(cond), ren(thenBr), ren(elseBr))
      case Cast(expr, tpe) =>
        Cast(ren(expr), tpe)
      case Sequence(stats, exprOpt) =>
        Sequence(stats.map(rename(_, ctx)), exprOpt.map(ren))
      case VariableRef(name) => {
        val newName = ctx.currNameFor(name)
        VariableRef(newName)
      }
    }
    res.setType(expr.getType)
  }

  private def rename(block: Block, ctx: LocalVarsCtx): Block = {
    val Block(stats) = block
    Block(stats.map(rename(_, ctx)))
  }

  private def rename(statement: Statement, ctx: LocalVarsCtx): Statement = {
    statement match
      case expr: Expr => rename(expr, ctx)
      case Block(stats) =>
        val newCtx = ctx.copied
        Block(stats.map(rename(_, newCtx)))
      case LocalDef(localName, optType, rhs, isReassignable) => {
        val renRhs = rename(rhs, ctx)
        val tpe = optType.getOrElse(rhs.getType)
        val newName = ctx.addAndRetNameFor(localName, tpe)
        LocalDef(newName, optType, renRhs, isReassignable)
      }
      case VarAssig(lhs, rhs) =>
        VarAssig(rename(lhs, ctx), rename(rhs, ctx))
      case IfThenElse(cond, thenBr, elseBrOpt) =>
        IfThenElse(rename(cond, ctx), rename(thenBr, ctx), elseBrOpt.map(rename(_, ctx)))
      case WhileLoop(cond, body, invariants) =>
        assert(invariants.isEmpty)
        val renBody = rename(body, ctx)
        WhileLoop(rename(cond, ctx), renBody, Nil)
      case ReturnStat(optVal) =>
        ReturnStat(optVal.map(rename(_, ctx)))
      case PanicStat(msg) =>
        PanicStat(rename(msg, ctx))
      case Assertion(formulaExpr, descr, isAssumed) =>
        Assertion(rename(formulaExpr, ctx), descr, isAssumed).setPositionSp(statement.getPosition)
      case _: (VarModif | ForLoop) => assert(false)
  }

  private def rename(param: Param, ctx: LocalVarsCtx): Param = {
    val Param(paramName, tpe) = param
    val newName = ctx.addAndRetNameFor(paramName, tpe)
    Param(newName, tpe)
  }

}
