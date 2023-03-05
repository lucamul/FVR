package compiler.desugarer

import compiler.irs.Asts.*
import compiler.prettyprinter.PrettyPrinter
import compiler.{AnalysisContext, CompilerStep, FunctionalChecker, FunctionsToInject, Replacer}
import lang.Operator.*
import lang.{Operator, Operators}
import lang.Types.PrimitiveType.*
import lang.Types.{ArrayType, UndefinedType}
import lang.SoftKeywords.Result

/**
 * Desugaring replaces:
 *  - `x <= y` ---> `(x < y) || (x == y)` if x,y are Doubles
 *  - `x != y` ---> `!(x == y)` if `mode.desugarOperators`
 *  - `VarModif`: `x += y` ---> `x = x + y`
 *  - `for` ---> `while`
 *  - `-x` ---> `0 - x` if `mode.desugarOperators`
 *  - `!x` ---> `when x then false else true` if `mode.desugarOperators`
 *  - `x && y` ---> `when x then y else false`
 *  - `x || y` ---> `when x then true else y`
 *  - `[x_1, ... , x_n]` ---> `val $0 = arr Int[n]; $0[0] = x_1; ... ; $0[n-1] = x_n; $0`
 *  - Preconditions ---> assertions at call site and assumptions at the beginning of the body of the function
 *  - Postconditions ---> assumptions at call site and assertions on each exit point of the function
 *  - Loop invariants ---> assertions ; loop ; assumptions, with the new body = { assumptions ; old body ; assumptions }
 *  - Addition of assumptions for control-flow structures: e.g. assume if condition in then branch
 */
final class Desugarer(mode: Desugarer.Mode)
  extends CompilerStep[(List[Source], AnalysisContext), (List[Source], AnalysisContext)] {

  private val uniqueIdGenerator = new UniqueIdGenerator("$")

  override def apply(input: (List[Source], AnalysisContext)): (List[Source], AnalysisContext) = {
    val (sources, ctx) = input
    val desugaredSources = sources.map(desugar(_)(ctx))
    desugaredSources.foreach(_.assertAllTypesAreSet())
    (desugaredSources, ctx)
  }

  private def desugar(src: Source)(implicit ctx: AnalysisContext): Source = Source(src.defs.map(desugar)).setName(src.getName)

  private def desugar(block: Block)(implicit ctx: AnalysisContext): Block = Block(block.stats.map(desugar))

  private def desugar(funDef: FunDef)(implicit ctx: AnalysisContext): FunDef = {
    val Block(bodyStats) = desugar(funDef.body)
    val newBodyStats = funDef.precond.map(formula =>
      desugar(assumption(formula, PrettyPrinter.prettyPrintStat(formula)).setPositionSp(funDef.getPosition))
    ) ++ bodyStats
    val postcondWithRenaming = funDef.postcond.map(formula =>
      desugar(assertion(
        formula,
        PrettyPrinter.prettyPrintStat(formula)
      ))
    )
    FunDef(funDef.funName, funDef.params.map(desugar), funDef.optRetType,
      addAssertsOnRetVals(Block(newBodyStats))(postcondWithRenaming), Nil, Nil, funDef.verifIgnore)
  }

  private def desugar(structDef: StructDef)(implicit ctx: AnalysisContext): StructDef = {
    StructDef(structDef.structName, structDef.fields.map(desugar))
  }

  private def desugar(param: Param): Param = param

  private def desugar(localDef: LocalDef)(implicit ctx: AnalysisContext): LocalDef =
    LocalDef(localDef.localName, localDef.optType, desugar(localDef.rhs), localDef.isReassignable)

  private def desugar(varAssig: VarAssig)(implicit ctx: AnalysisContext): VarAssig = VarAssig(desugar(varAssig.lhs), desugar(varAssig.rhs))

  private def desugar(varModif: VarModif)(implicit ctx: AnalysisContext): VarAssig = {
    val VarModif(lhs, rhs, op) = varModif
    desugar(VarAssig(lhs, BinaryOp(lhs, op, rhs).setType(lhs.getType)))
  }

  private def desugar(ifThenElse: IfThenElse)(implicit ctx: AnalysisContext): IfThenElse = {
    val newThenBr = blockify(
      List(desugar(assumption(ifThenElse.cond, "then assumption"))),
      desugar(ifThenElse.thenBr),
      Nil
    )
    val elseBrAssumption = desugar(assumption(not(ifThenElse.cond), "else assumption"))
    val newElseBr = ifThenElse.elseBrOpt.map { elseBr =>
      desugar(blockify(
        List(elseBrAssumption),
        elseBr,
        Nil
      ))
    }.getOrElse(elseBrAssumption)
    IfThenElse(desugar(ifThenElse.cond), newThenBr, Some(newElseBr))
  }

  private def desugar(whileLoop: WhileLoop)(implicit ctx: AnalysisContext): Statement = {
    val invariants = whileLoop.invariants.map(invar =>
      assertion(
        invar,
        "invariant " ++ PrettyPrinter.prettyPrintStat(invar)
      ).setPositionSp(invar.getPosition)
    )
    val invarAssumed = invariants.map(_.copy(isAssumed = true))
    val whileBodyAssumptions = assumption(whileLoop.cond, "while body")
    val newBody = blockify(whileBodyAssumptions :: invarAssumed, whileLoop.body, invariants)
    val desugaredLoop = WhileLoop(desugar(whileLoop.cond), desugar(newBody), Nil)
    val whileEndAssumption = assumption(not(whileLoop.cond), "while end")
    blockify(invariants.map(desugar), desugaredLoop, desugar(whileEndAssumption) :: invarAssumed.map(desugar))
  }

  private def desugar(forLoop: ForLoop)(implicit ctx: AnalysisContext): Block = {
    val body = Block(
      forLoop.body.stats ++ forLoop.stepStats
    )
    val stats: List[Statement] = forLoop.initStats :+ WhileLoop(forLoop.cond, body, forLoop.invariants)
    Block(stats.map(desugar))
  }

  private def desugar(returnStat: ReturnStat)(implicit ctx: AnalysisContext): ReturnStat = {
    val newRetStat = ReturnStat(returnStat.optVal.map(desugar))
    newRetStat.setPosition(returnStat.getPosition)
    newRetStat
  }

  private def desugar(panicStat: PanicStat)(implicit ctx: AnalysisContext): Statement = {
    if mode.desugarPanic then Assertion(BoolLit(false), PrettyPrinter.prettyPrintStat(panicStat), isAssumed = false)
    else PanicStat(desugar(panicStat.msg))
  }

  private def desugar(assertion: Assertion)(implicit ctx: AnalysisContext): Assertion = {
    Assertion(desugar(assertion.formulaExpr), assertion.descr, assertion.isAssumed)
  }

  private def desugar(expr: Expr)(implicit ctx: AnalysisContext): Expr = {
    val desugared = expr match {
      case literal: Literal => literal
      case varRef: VariableRef => varRef

      case indexing: Indexing => {
        val desugaredIndexing = Indexing(desugar(indexing.indexed), desugar(indexing.arg)).setType(indexing.getType)
        if (mode.checkArrayAccesses) {
          Sequence(List(
            Assertion(desugar(BinaryOp(
              BinaryOp(IntLit(0), LessOrEq, indexing.arg).setType(BoolType),
              And,
              BinaryOp(indexing.arg, LessThan, UnaryOp(Sharp, indexing.indexed).setType(IntType)).setType(BoolType)
            ).setType(BoolType)),
              "array access ",
              isAssumed = false
            ).setPositionSp(indexing.getPosition)
          ), Some(desugaredIndexing))
        } else {
          desugaredIndexing
        }
      }

      case arrayInit: ArrayInit => ArrayInit(arrayInit.elemType, desugar(arrayInit.size))
      case structInit: StructInit => StructInit(structInit.structName, structInit.args.map(desugar))

      case call@Call(calleeName, args) =>
        val funInfo = ctx.functions(calleeName)
        if (funInfo.isBuiltin) {
          Call(calleeName, args.map(desugar)).setType(funInfo.sig.retType)
        } else {
          val argsUids = for _ <- funInfo.sig.argTypes yield uniqueIdGenerator.next()
          val argsLocalDefsAndRefs = funInfo.sig.argTypes.indices.map { idx =>
            val uid = argsUids(idx)
            val tpe = funInfo.sig.argTypes(idx)
            (
              LocalDef(uid, Some(tpe), desugar(args(idx)), isReassignable = false),
              VariableRef(uid).setType(tpe)
            )
          }.toList
          val argsLocalDefinitions = argsLocalDefsAndRefs.map(_._1)
          val argsLocalReferences = argsLocalDefsAndRefs.map(_._2)
          val resultUid = uniqueIdGenerator.next()
          // can do optDef.get because only built-in functions can have None as a funDef, and built-ins do not have postconditions
          // can call zip because the typechecker checked the number of arguments
          val resultLocalRef = VariableRef(resultUid).setType(funInfo.sig.retType)
          val argsRenameMap = {
            funInfo.optDef.get.params
              .map(_.paramName)
              .zip(argsLocalReferences)
              .toMap + (Result.str -> resultLocalRef) // resultLocalDef is ignored if return type is Void
          }
          val retIsNoValType = funInfo.sig.retType.isNoValType
          val newCall = Call(calleeName, argsLocalReferences).setType(funInfo.sig.retType)
          val newCallLine = {
            if retIsNoValType then newCall
            else LocalDef(resultUid, Some(funInfo.sig.retType), newCall, isReassignable = false)
          }
          val allPostcond = funInfo.postcond ++ funInfo.optDef.flatMap(generateTrivialPostcond)
          val stats = {
            argsLocalDefinitions ++
              funInfo.precond.map(formula =>
                desugar(assertion(
                  Replacer.replaceInExpr(formula, argsRenameMap),
                  "precond  " ++ PrettyPrinter.prettyPrintStat(formula)
                ).setPositionSp(call.getPosition))
              ) ++
              List(newCallLine) ++
              allPostcond.map(formula =>
                desugar(assumption(
                  Replacer.replaceInExpr(formula, argsRenameMap),
                  PrettyPrinter.prettyPrintStat(formula)
                ).setPositionSp(call.getPosition))
              )
          }
          Sequence(stats, if retIsNoValType then None else Some(resultLocalRef))
        }

      // [x_1, ... , x_n] ---> explicit assignments
      case filledArrayInit@FilledArrayInit(arrayElems) =>
        val arrayType = filledArrayInit.getType.asInstanceOf[ArrayType]
        val elemType = arrayType.elemType
        val arrValId = uniqueIdGenerator.next()
        val arrValRef = VariableRef(arrValId).setType(arrayType)
        val arrInit = ArrayInit(elemType, IntLit(arrayElems.size)).setType(filledArrayInit.getType)
        val arrayValDefinition = LocalDef(arrValId, Some(arrayType), arrInit, isReassignable = false)
        val arrElemAssigStats = arrayElems.map(desugar).zipWithIndex.map {
          (elem, idx) => VarAssig(Indexing(arrValRef, IntLit(idx)).setType(arrayType.elemType), elem)
        }
        Sequence(desugar(arrayValDefinition) :: arrElemAssigStats, Some(desugar(arrValRef)))

      case unaryOp@UnaryOp(operator, operand) =>
        if mode.desugarOperators then desugarUnaryOp(unaryOp)
        else UnaryOp(operator, desugar(operand))

      case BinaryOp(lhs, Equality, rhs) if lhs.getType.subtypeOf(StringType) => {
        val desugaredLhs = desugar(lhs)
        val desugaredRhs = desugar(rhs)
        if mode.desugarStringEq then Call(
          FunctionsToInject.stringEqualityMethodName,
          List(desugaredLhs, desugaredRhs)
        ).setType(BoolType)
        else BinaryOp(desugaredLhs, Equality, desugaredRhs)
      }

      case binaryOp: BinaryOp =>
        desugarBinaryOp(binaryOp)

      case select: Select => Select(desugar(select.lhs), select.selected)

      // need to treat separately the case where one of the branches does not return (o.w. Java ASM crashes)
      case Ternary(cond, thenBr, elseBr) if thenBr.getType == NothingType || elseBr.getType == NothingType => {
        if (thenBr.getType == NothingType) {
          val ifStat = IfThenElse(cond, thenBr, None)
          desugar(Sequence(List(ifStat), Some(elseBr)))
        } else {
          val ifStat = IfThenElse(UnaryOp(ExclamationMark, cond).setType(BoolType), elseBr, None)
          desugar(Sequence(List(ifStat), Some(thenBr)))
        }
      }
      case Ternary(cond, thenBr, elseBr) =>
        Ternary(
          desugar(cond),
          desugar(sequencify(List(assumption(cond, "then assumption")), thenBr)),
          desugar(sequencify(List(assumption(not(cond), "else assumption")), elseBr))
        )
      case Cast(expr, tpe) => Cast(desugar(expr), tpe)
      case Sequence(stats, exprOpt) => Sequence(stats.map(desugar), exprOpt.map(desugar))
    }
    desugared.setTypeOpt(expr.getTypeOpt)
  }

  private def desugarBinaryOp(binaryOp: BinaryOp)(implicit ctx: AnalysisContext): Expr = {
    val isDoubleOp = binaryOp.lhs.getType == DoubleType || binaryOp.rhs.getType == DoubleType
    // always desugar && and || because of lazy evaluation
    binaryOp.operator match {

      // x <= y ---> x <= y || x == y
      case LessOrEq if isDoubleOp && mode.desugarOperators =>
        makeDoubleCompOrEq(desugar(binaryOp.lhs), LessThan, desugar(binaryOp.rhs))

      case GreaterOrEq if isDoubleOp && mode.desugarOperators =>
        makeDoubleCompOrEq(desugar(binaryOp.lhs), GreaterThan, desugar(binaryOp.rhs))

      // x != y ---> !(x == y)
      case Inequality if mode.desugarOperators =>
        desugar(UnaryOp(ExclamationMark,
          BinaryOp(binaryOp.lhs, Equality, binaryOp.rhs).setType(BoolType)
        ).setType(BoolType))

      // x && y ---> when x then y else false
      case And if mode.desugarOperators || !FunctionalChecker.isPurelyFunctional(binaryOp.rhs) =>
        desugar(Ternary(binaryOp.lhs, binaryOp.rhs, BoolLit(false)))

      // x || y ---> when x then true else y
      case Or if mode.desugarOperators || !FunctionalChecker.isPurelyFunctional(binaryOp.rhs) =>
        desugar(Ternary(binaryOp.lhs, BoolLit(true), binaryOp.rhs))

      // nothing to desugar at top-level, only perform recursive calls
      case _ => BinaryOp(desugar(binaryOp.lhs), binaryOp.operator, desugar(binaryOp.rhs))
    }
  }

  private def makeDoubleCompOrEq(desugaredLhs: Expr, strictCompOp: Operator, desugaredRhs: Expr)(implicit analysisContext: AnalysisContext) = {
    require(strictCompOp == LessThan || strictCompOp == GreaterThan)
    val lhsLocalName = uniqueIdGenerator.next()
    val rhsLocalName = uniqueIdGenerator.next()
    val lhsLocalRef = VariableRef(lhsLocalName).setType(DoubleType)
    val rhsLocalRef = VariableRef(rhsLocalName).setType(DoubleType)
    Sequence(List(
      LocalDef(lhsLocalName, Some(DoubleType), desugaredLhs, isReassignable = false),
      LocalDef(rhsLocalName, Some(DoubleType), desugaredRhs, isReassignable = false)
    ), Some(
      desugar(BinaryOp(
        BinaryOp(lhsLocalRef, strictCompOp, rhsLocalRef).setType(BoolType),
        Or,
        BinaryOp(lhsLocalRef, Equality, rhsLocalRef).setType(BoolType)
      ).setType(BoolType))
    ))
  }

  private def desugarUnaryOp(unaryOp: UnaryOp)(implicit ctx: AnalysisContext): Expr = {
    val UnaryOp(operator, operand) = unaryOp
    val desugaredOperand = desugar(operand)
    operator match {
      case Minus if operand.getType == IntType => BinaryOp(IntLit(0), Minus, desugaredOperand)
      case Minus if operand.getType == DoubleType => BinaryOp(DoubleLit(0.0), Minus, desugaredOperand)
      case ExclamationMark => {
        desugaredOperand match {
          case UnaryOp(ExclamationMark, subOperand) => subOperand
          case _ => BinaryOp(desugaredOperand, Equality, BoolLit(false))
        }
      }
      case _ => UnaryOp(operator, desugaredOperand)
    }
  }

  private def desugar(statement: Statement)(implicit ctx: AnalysisContext): Statement = {
    // call appropriate method for each type of statement
    statement match
      case expr: Expr => desugar(expr)
      case block: Block => desugar(block)
      case localDef: LocalDef => desugar(localDef)
      case varAssig: VarAssig => desugar(varAssig)
      case varModif: VarModif => desugar(varModif)
      case ifThenElse: IfThenElse => desugar(ifThenElse)
      case whileLoop: WhileLoop => desugar(whileLoop)
      case forLoop: ForLoop => desugar(forLoop)
      case returnStat: ReturnStat => desugar(returnStat)
      case panicStat: PanicStat => desugar(panicStat)
      case assertion: Assertion => desugar(assertion)
  }

  private def desugar(topLevelDef: TopLevelDef)(implicit ctx: AnalysisContext): TopLevelDef = {
    topLevelDef match
      case funDef: FunDef => desugar(funDef)
      case structDef: StructDef => desugar(structDef)
  }

  private def addAssertsOnRetVals[S <: Statement](stat: S)(implicit rawAssertions: List[Assertion]): S = {
    if (rawAssertions.isEmpty) {
      stat
    } else {
      val resStat: Statement = stat match {
        case Block(stats) => Block(stats.map(addAssertsOnRetVals))
        case LocalDef(localName, optType, rhs, isReassignable) =>
          LocalDef(localName, optType, addAssertsOnRetVals(rhs), isReassignable)
        case VarAssig(lhs, rhs) => VarAssig(addAssertsOnRetVals(lhs), addAssertsOnRetVals(rhs))
        case VarModif(lhs, rhs, op) => VarModif(addAssertsOnRetVals(lhs), addAssertsOnRetVals(rhs), op)
        case IfThenElse(cond, thenBr, elseBrOpt) =>
          IfThenElse(addAssertsOnRetVals(cond), addAssertsOnRetVals(thenBr), elseBrOpt.map(addAssertsOnRetVals))
        case WhileLoop(cond, body, invariants) =>
          WhileLoop(addAssertsOnRetVals(cond), addAssertsOnRetVals(body), invariants.map(addAssertsOnRetVals))
        case ForLoop(initStats, cond, stepStats, body, invariants) =>
          ForLoop(
            initStats.map(addAssertsOnRetVals),
            addAssertsOnRetVals(cond),
            stepStats.map(addAssertsOnRetVals),
            addAssertsOnRetVals(body),
            invariants.map(addAssertsOnRetVals)
          )
        case PanicStat(msg) => PanicStat(addAssertsOnRetVals(msg))
        case Assertion(formulaExpr, descr, isAssumed) =>
          Assertion(addAssertsOnRetVals(formulaExpr), descr, isAssumed).setPositionSp(stat.getPosition)
        case literal: Literal => literal
        case variableRef: VariableRef => variableRef
        case Call(callee, args) => Call(callee, args.map(addAssertsOnRetVals))
        case Indexing(indexed, arg) =>
          Indexing(addAssertsOnRetVals(indexed), addAssertsOnRetVals(arg)).setPositionSp(stat.getPosition)
        case ArrayInit(elemType, size) => ArrayInit(elemType, addAssertsOnRetVals(size))
        case FilledArrayInit(arrayElems) => FilledArrayInit(arrayElems.map(addAssertsOnRetVals))
        case StructInit(structName, args) => StructInit(structName, args.map(addAssertsOnRetVals))
        case UnaryOp(operator, operand) => UnaryOp(operator, addAssertsOnRetVals(operand))
        case BinaryOp(lhs, operator, rhs) =>
          BinaryOp(addAssertsOnRetVals(lhs), operator, addAssertsOnRetVals(rhs))
        case Select(lhs, selected) => Select(addAssertsOnRetVals(lhs), selected)
        case Ternary(cond, thenBr, elseBr) =>
          Ternary(addAssertsOnRetVals(cond), addAssertsOnRetVals(thenBr), addAssertsOnRetVals(elseBr))
        case Cast(expr, tpe) => Cast(addAssertsOnRetVals(expr), tpe)
        case Sequence(stats, exprOpt) => Sequence(stats.map(addAssertsOnRetVals), exprOpt.map(addAssertsOnRetVals))
        case retNone@ReturnStat(None) => retNone

        case retStat@ReturnStat(Some(retVal)) =>
          val uid = uniqueIdGenerator.next()
          val newLocalRef = VariableRef(uid).setType(retVal.getType)
          val renamedAssertions = {
            rawAssertions
              .map(_.copy().setPositionSp(retStat.getPosition))
              .map(assertion =>
                Assertion(
                  formulaExpr = Replacer.replaceInExpr(assertion.formulaExpr, Map(Result.str -> newLocalRef)),
                  "postcond " ++ assertion.descr,
                  isAssumed = false
                )
              )
          }
          blockify(
            LocalDef(uid, retVal.getTypeOpt, retVal, isReassignable = false) :: renamedAssertions,
            ReturnStat(Some(newLocalRef)),
            Nil
          )
      }
      assert(resStat.isInstanceOf[Expr] == stat.isInstanceOf[Expr])
      (stat, resStat) match {
        case (expr: Expr, resExpr: Expr) =>
          resExpr.setType(expr.getType)
        case _ => ()
      }
      resStat.asInstanceOf[S]
    }
  }

  private def generateTrivialPostcond(funDef: FunDef)(implicit analysisContext: AnalysisContext): Option[Expr] = {
    funDef.body match
      case Block(List(ReturnStat(Some(expr))))
        if expr.collect { case Call(callee, _) if callee == funDef.funName => () }.isEmpty
          && FunctionalChecker.isPurelyFunctional(expr)
      =>
        Some(BinaryOp(
          VariableRef(Result.str).setType(funDef.signature.retType),
          Equality,
          expr
        ).setType(BoolType))
      case _ => None
  }

  private def not(expr: Expr): Expr = {
    UnaryOp(ExclamationMark, expr).setType(BoolType)
  }

  private def blockify(before: List[Statement], possiblyBlock: Statement, after: List[Statement]): Block = {
    possiblyBlock match {
      case block: Block if before.isEmpty && after.isEmpty => block
      case Block(stats) => Block(before ++ stats ++ after)
      case middleStat => Block(before ++ List(middleStat) ++ after)
    }
  }

  private def sequencify(before: List[Statement], possiblySeq: Expr): Expr = {
    possiblySeq match {
      case Sequence(stats, exprOpt) =>
        Sequence(before ++ stats, exprOpt)
      case notSeq =>
        Sequence(before, Some(notSeq))
    }
  }

  private def assumption(formula: Expr, descr: String)(implicit analysisContext: AnalysisContext): Assertion = {
    Assertion(formula, descr, isAssumed = true)
  }

  private def assertion(formula: Expr, descr: String)(implicit analysisContext: AnalysisContext): Assertion = {
    Assertion(formula, descr, isAssumed = false)
  }

}

object Desugarer {

  enum Mode(
             val desugarOperators: Boolean,
             val desugarStringEq: Boolean,
             val desugarPanic: Boolean,
             val checkArrayAccesses: Boolean
           ) {
    case Compile extends Mode(
      desugarOperators = true,
      desugarStringEq = true,
      desugarPanic = false,
      checkArrayAccesses = false
    )
    case Verify extends Mode(
      desugarOperators = false,
      desugarStringEq = false,
      desugarPanic = true,
      checkArrayAccesses = true
    )
  }

}
