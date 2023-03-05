package compiler.typechecker

import compiler.CompilationStep.TypeChecking
import compiler.Errors.{CompilationError, Err, ErrorReporter, Warning}
import compiler.FunctionalChecker.isPurelyFunctional
import compiler.irs.Asts.*
import compiler.{AnalysisContext, CompilerStep, FunctionalChecker, Position}
import lang.Operator.{Equality, Inequality, Sharp}
import lang.SoftKeywords.Result
import lang.Types.*
import lang.Types.PrimitiveType.*
import lang.{Operators, TypeConversion}

final class TypeChecker(errorReporter: ErrorReporter) extends CompilerStep[(List[Source], AnalysisContext), (List[Source], AnalysisContext)] {


  override def apply(input: (List[Source], AnalysisContext)): (List[Source], AnalysisContext) = {
    val (sources, analysisContext) = input
    for src <- sources do {
      check(src, TypeCheckingContext(analysisContext))
    }
    errorReporter.displayAndTerminateIfErrors()
    sources.foreach(_.assertAllTypesAreSet())
    input
  }

  private def check(ast: Ast, ctx: TypeCheckingContext): Type = {
    val tpe = ast match {

      case Source(defs) =>
        for df <- defs do {
          check(df, ctx.copyWithoutLocals)
        }
        VoidType

      case Block(stats) =>
        val newCtx = ctx.copied
        for stat <- stats do {
          check(stat, newCtx)
        }
        VoidType

      case StructDef(_, fields) =>
        for param@Param(_, tpe) <- fields if !ctx.analysisContext.knowsType(tpe) do {
          reportError(s"unknown type: $tpe", param.getPosition)
        }
        VoidType

      case funDef@FunDef(funName, params, optRetType, body, precond, postcond, _) =>
        optRetType.foreach { retType =>
          if (!ctx.analysisContext.knowsType(retType)) {
            reportError(s"return type is unknown: '$retType'", funDef.getPosition)
          }
        }
        checkFunctionBody(ctx, funName, params, body)
        val expRetType = optRetType.getOrElse(VoidType)
        checkFuncReturnStats(funDef, funName, body, expRetType)
        if (funDef.params.exists(_.paramName == Result.str)) {
          errorReporter.push(Err(TypeChecking, s"function arguments cannot be named '$Result'", funDef.getPosition))
        }
        checkPreAndPostcond(ctx, funDef, params, precond, postcond, expRetType)
        checkNoRecInFormulas(funDef)
        VoidType

      case localDef@LocalDef(localName, optType, rhs, isReassignable) =>
        val inferredType = check(rhs, ctx)
        optType.foreach { expType =>
          val typeIsKnown = ctx.analysisContext.knowsType(expType)
          if (!typeIsKnown) {
            reportError(s"unknown type: $expType", localDef.getPosition)
          } else if (!inferredType.subtypeOf(expType)) {
            reportError(s"${localDef.keyword} should be of type '$expType', found '$inferredType'", localDef.getPosition)
          }
        }
        val actualType = optType.getOrElse(inferredType)
        localDef.optType = Some(actualType)
        ctx.addLocal(localName, actualType, isReassignable, duplicateVarCallback = { () =>
          reportError(s"'$localName' is already defined in this scope", localDef.getPosition)
        }, forbiddenTypeCallback = { () =>
          reportError(s"${localDef.keyword} '$localName' has type '$actualType', which is forbidden", localDef.getPosition)
        })
        VoidType

      case _: IntLit => IntType
      case _: DoubleLit => DoubleType
      case _: CharLit => CharType
      case _: BoolLit => BoolType
      case _: StringLit => StringType

      case varRef@VariableRef(name) =>
        ctx.locals.get(name) match {
          case Some((tpe, _)) => tpe
          case None =>
            reportError(s"not found: '$name'", varRef.getPosition)
        }

      case call@Call(calleeName, args) =>
        ctx.functions.get(calleeName).map(_.sig) match {
          case Some(funSig) =>
            checkCallArgs(funSig.argTypes, args, ctx, call.getPosition)
            funSig.retType

          case None => reportError(s"not found: $calleeName", call.getPosition)
        }

      case Indexing(indexed, arg) =>
        val indexedType = check(indexed, ctx)
        val indexedError = !indexedType.isInstanceOf[ArrayType]
        if (indexedError) {
          reportError("indexed expression is not an array", indexed.getPosition)
        }
        val argType = check(arg, ctx)
        val argError = !argType.subtypeOf(IntType)
        if (argError) {
          reportError(s"indexing expression should be of type '${IntType.str}', found '$argType'", arg.getPosition)
        }
        if indexedError then UndefinedType else indexedType.asInstanceOf[ArrayType].elemType

      case arrayInit@ArrayInit(elemType, size) =>
        if (elemType.isNoValType) {
          reportError(s"array cannot have element type $elemType", arrayInit.getPosition)
        }
        val sizeType = check(size, ctx)
        if (!sizeType.subtypeOf(IntType)) {
          reportError(s"array size should be of type '${IntType.str}', found '$sizeType'", size.getPosition)
        }
        size match {
          case IntLit(value) if value < 0 =>
            reportError("array size should be nonnegative", size.getPosition)
          case _ => // do nothing
        }
        ArrayType(elemType)

      case filledArrayInit@FilledArrayInit(Nil) =>
        reportError("cannot infer type of empty array, use 'arr <type>[<size>]' instead", filledArrayInit.getPosition)

      case filledArrayInit@FilledArrayInit(arrayElems) =>
        val types = arrayElems.map(check(_, ctx))
        types.find(tpe => types.forall(_.subtypeOf(tpe))) match {
          case Some(inferredElemType) => ArrayType(inferredElemType)
          case None => reportError("cannot infer array type", filledArrayInit.getPosition)
        }

      case structInit@StructInit(structName, args) =>
        ctx.structs.get(structName) match {
          case Some(structSig) =>
            checkCallArgs(structSig.fields.values.toList, args, ctx, structInit.getPosition)
            StructType(structName)
          case None => reportError(s"not found: struct '$structName'", structInit.getPosition)
        }

      case unaryOp@UnaryOp(operator, operand) =>
        val operandType = check(operand, ctx)
        if (operator == Sharp) {
          if (operandType.isInstanceOf[ArrayType] || operandType == StringType) {
            IntType
          } else {
            reportError("operator # can only be applied to arrays and strings", unaryOp.getPosition)
          }
        } else {
          Operators.unaryOperatorSignatureFor(operator, operandType) match {
            case Some(sig) => sig.retType
            case None =>
              reportError(s"no definition of operator '$operator' found for operand '$operandType'", unaryOp.getPosition)
          }
        }

      case binOp@BinaryOp(lhs, operator, rhs) =>
        val lhsType = check(lhs, ctx)
        val rhsType = check(rhs, ctx)
        if (operator == Equality || operator == Inequality) {
          if (lhsType != rhsType) {
            reportError(s"cannot compare '$lhsType' and '$rhsType' using ${Equality.str} or ${Inequality.str}", binOp.getPosition)
          }
          BoolType
        } else {
          Operators.binaryOperatorSigFor(lhsType, operator, rhsType) match {
            case Some(sig) => sig.retType
            case None =>
              reportError(s"no definition of operator '$operator' found for operands '$lhsType' and '$rhsType'", binOp.getPosition)
          }
        }

      case select@Select(lhs, selected) =>
        val lhsType = check(lhs, ctx)
        lhsType match {
          case StructType(typeName) =>
            val structSig = ctx.structs.apply(typeName)
            structSig.fields.get(selected) match {
              case Some(fieldType) => fieldType
              case None => reportError(s"no '$selected' field found for type '$lhsType'", select.getPosition)
            }
          case _ => reportError(s"no '$selected' field found: not a struct", select.getPosition)
        }

      case varAssig@VarAssig(lhs, rhs) =>
        val rhsType = check(rhs, ctx)
        lhs match {
          case VariableRef(name) =>
            ctx.locals.get(name) match {
              case Some((tpe, mut)) if mut =>
                lhs.setType(tpe)
                if (!rhsType.subtypeOf(tpe)) {
                  reportError(s"cannot assign a value of type '$rhsType' to a variable of type $tpe", varAssig.getPosition)
                }
              case Some(_) =>
                reportError(s"cannot mutate '$name': not a var", varAssig.getPosition)
              case None =>
                reportError(s"not found: '$name'", varAssig.getPosition)
            }
          case indexing: Indexing =>
            val lhsType = check(indexing, ctx)
            if (!rhsType.subtypeOf(lhsType)) {
              reportError(s"cannot assign a value of type '$rhsType' to an array of element type '$lhsType'", indexing.getPosition)
            }
          case select: Select =>
            val lhsType = check(select, ctx)
            if (!rhsType.subtypeOf(lhsType)) {
              reportError(s"cannot assign a value of type '$rhsType' to a field of type '$lhsType'", select.getPosition)
            }
          case _ =>
            reportError("syntax error: only variables, struct fields and array elements can be assigned", varAssig.getPosition)
        }
        VoidType

      case varModif@VarModif(lhs, rhs, op) =>
        val rhsType = check(rhs, ctx)
        lhs match {
          case VariableRef(name) =>
            ctx.locals.get(name) match {
              case Some((tpe, mut)) if mut =>
                Operators.binaryOperatorSigFor(tpe, op, rhsType) match {
                  case Some(opSig) =>
                    lhs.setType(tpe)
                    if (!opSig.retType.subtypeOf(tpe)) {
                      reportError(s"$tpe ${op.str} $rhsType ==> ${opSig.retType}, not ${op.str}", varModif.getPosition)
                    }
                  case None =>
                    reportError(s"no definition of operator '$op' found for operands '$tpe' and '$rhsType'", varModif.getPosition)
                }
              case Some(_) =>
                reportError(s"cannot mutate '$name': not a var", varModif.getPosition)
              case None =>
                reportError(s"not found: '$name'", varModif.getPosition)
            }
          case indexingOrSelect: (Indexing | Select) =>
            val lhsType = check(indexingOrSelect, ctx)
            Operators.binaryOperatorSigFor(lhsType, op, rhsType) match {
              case Some(opSig) =>
                if (!opSig.retType.subtypeOf(lhsType)) {
                  reportError(s"$lhsType ${op.str} $rhsType ==> ${opSig.retType}, not ${op.str}", varModif.getPosition)
                }
              case None =>
                reportError(s"no definition of operator '$op' found for operands '$lhsType' and '$rhsType'", varModif.getPosition)
            }
          case _ =>
            reportError("syntax error: only variables and array elements can be assigned", varModif.getPosition)
        }
        VoidType

      case IfThenElse(cond, thenBr, elseBrOpt) =>
        checkControlFlowCond(cond, ctx)
        check(thenBr, ctx)
        elseBrOpt.foreach(check(_, ctx))
        VoidType

      case ternary@Ternary(cond, thenBr, elseBr) =>
        checkControlFlowCond(cond, ctx)
        val thenType = check(thenBr, ctx)
        val elseType = check(elseBr, ctx)
        val thenIsSupertype = elseType.subtypeOf(thenType)
        val thenIsSubtype = thenType.subtypeOf(elseType)
        if (thenIsSupertype) {
          thenType
        } else if (thenIsSubtype) {
          elseType
        } else {
          reportError(s"type mismatch in ternary operator: first branch has type $thenType, second has type $elseType", ternary.getPosition)
        }

      case whileLoop@WhileLoop(cond, body, invariants) =>
        checkControlFlowCond(cond, ctx)
        check(body, ctx)
        checkVerificationFormulas(invariants.map((_, whileLoop.getPosition)), ctx)
        VoidType

      case forLoop@ForLoop(initStats, cond, stepStats, body, invariants) =>
        val newCtx = ctx.copied
        initStats.foreach(check(_, newCtx))
        checkControlFlowCond(cond, newCtx)
        stepStats.foreach(check(_, newCtx))
        check(body, newCtx)
        checkVerificationFormulas(invariants.map((_, forLoop.getPosition)), newCtx)
        VoidType

      case ReturnStat(valueOpt) =>
        valueOpt.foreach(check(_, ctx))
        VoidType

      case cast@Cast(expr, tpe) =>
        val exprTpe = check(expr, ctx)
        if (exprTpe.subtypeOf(tpe)) {
          reportError(s"useless conversion: '$exprTpe' --> '$tpe'", cast.getPosition, isWarning = true)
          tpe
        } else if (TypeConversion.conversionFor(exprTpe, tpe).isDefined) {
          tpe
        } else {
          reportError(s"cannot cast ${expr.getType} to $tpe", cast.getPosition)
        }

      case panicStat@PanicStat(msg) =>
        val msgType = check(msg, ctx)
        if (!msgType.subtypeOf(StringType)) {
          reportError(s"panic can only be applied to type '${StringType.str}', found '$msgType'", panicStat.getPosition)
        }
        NothingType

      case assertion@Assertion(formulaExpr, _, _) =>
        checkVerificationFormulas(List((formulaExpr, assertion.getPosition)), ctx)
        VoidType

      case _: (Param | Sequence) => assert(false)
    }
    // if expression save type
    ast match {
      case expr: Expr => expr.setType(tpe)
      case _ => ()
    }
    tpe
  }

  private def checkFuncReturnStats(funDef: FunDef, funName: String, body: Block, expRetType: Type) = {
    val endStatus = checkReturns(body)
    if (!endStatus.alwaysStopped && !expRetType.subtypeOf(VoidType)) {
      reportError("missing return in non-Void function", funDef.getPosition)
    }
    val faultyTypes = endStatus.returned.filter(!_.subtypeOf(expRetType))
    if (faultyTypes.nonEmpty) {
      val faultyTypeStr = faultyTypes.map("'" + _ + "'").mkString(", ")
      reportError(s"function '$funName' should return '$expRetType', found $faultyTypeStr", funDef.getPosition)
    }
    if (expRetType == NothingType && !endStatus.alwaysStopped){
      reportError(s"cannot prove that function '$funName' with return type '$NothingType' cannot return", funDef.getPosition)
    }
  }

  private def checkFunctionBody(ctx: TypeCheckingContext, funName: String, params: List[Param], body: Block) = {
    val ctxWithParams = ctx.copyWithoutLocals
    for param <- params do {
      val typeIsKnown = ctx.analysisContext.knowsType(param.tpe)
      if (!typeIsKnown) {
        reportError(s"unknown type: ${param.tpe}", param.getPosition)
      }
      val paramType = if typeIsKnown then param.tpe else UndefinedType
      ctxWithParams.addLocal(param.paramName, paramType, false, duplicateVarCallback = { () =>
        reportError(s"identifier '${param.paramName}' is already used by another parameter of function '$funName'", param.getPosition)
      }, forbiddenTypeCallback = { () =>
        reportError(s"parameter '${param.paramName}' of function '$funName' has type '$paramType', which is forbidden", param.getPosition)
      })
    }
    check(body, ctxWithParams)
  }

  private def checkPreAndPostcond(
                                   ctx: TypeCheckingContext,
                                   funDef: FunDef,
                                   params: List[Param],
                                   precond: List[Expr],
                                   postcond: List[Expr],
                                   retType: Type
                                 ): Unit = {
    val precondCtx = ctx.copyWithoutLocals
    for param <- params do {
      precondCtx.addLocal(param.paramName, param.tpe, isReassignable = false, () => assert(false), () => assert(false))
    }
    checkVerificationFormulas(precond.map((_, funDef.getPosition)), precondCtx)
    val retTypeAdmitsPostcond = retType != VoidType && retType != NothingType
    if (retTypeAdmitsPostcond) {
      val postcondCtx = ctx.copyWithoutLocals
      for param <- params do {
        postcondCtx.addLocal(param.paramName, param.tpe, isReassignable = false, () => assert(false), () => assert(false))
      }
      postcondCtx.addLocal(Result.str, retType, isReassignable = false, () => assert(false), () => assert(false))
      checkVerificationFormulas(postcond.map((_, funDef.getPosition)), postcondCtx)
    } else if (postcond.nonEmpty) {
      reportError(s"postcondition on function with '${retType}' return type", funDef.getPosition)
    }
  }

  private def checkCallArgs(expTypes: List[Type], args: List[Expr], ctx: TypeCheckingContext, callPos: Option[Position]): Unit = {
    val expTypesIter = expTypes.iterator
    val argsIter = args.iterator
    var errorFound = false
    while (expTypesIter.hasNext && argsIter.hasNext && !errorFound) {
      val expType = expTypesIter.next()
      val arg = argsIter.next()
      val actType = check(arg, ctx)
      if (!actType.subtypeOf(expType)) {
        reportError(s"expected '$expType', found '$actType'", arg.getPosition)
        errorFound = true
      }
    }
    if (expTypesIter.hasNext) {
      reportError(s"expected argument of type '${expTypesIter.next()}', found end of arguments list", callPos)
    }
    if (argsIter.hasNext) {
      val arg = argsIter.next()
      reportError(s"expected end of arguments list, found argument of type '${check(arg, ctx)}'", arg.getPosition)
      for arg <- argsIter do {
        check(arg, ctx)
      }
    }
  }

  private def checkNoRecInFormulas(funDef: FunDef): Unit = {
    funDef.precond.foreach(checkNoRecInFormula(_, funDef))
    funDef.postcond.foreach(checkNoRecInFormula(_, funDef))
    funDef.collect {
      case Assertion(formulaExpr, _, _) =>
        checkNoRecInFormula(formulaExpr, funDef)
      case whileLoop: WhileLoop =>
        whileLoop.invariants.foreach(checkNoRecInFormula(_, funDef))
      case forLoop: ForLoop =>
        forLoop.invariants.foreach(checkNoRecInFormula(_, funDef))
    }
  }

  private def checkNoRecInFormula(formula: Expr, enclosingFunc: FunDef): Unit = {
    if (formula.collect { case Call(callee, _) if callee == enclosingFunc.funName => () }.nonEmpty){
      reportError("recursive calls are forbidden in formulas", formula.getPosition)
    }
  }

  /**
   * @param returned      types of all the expressions found after a `return`
   * @param alwaysStopped indicates whether the control-flow can reach the end of the considered construct without
   *                      encountering an instruction that terminates the function (`return` or `panic`)
   */
  private case class EndStatus(returned: Set[Type], alwaysStopped: Boolean)

  /**
   * Traverses the program, looking for instructions that end the function they are in (`return` and `panic`)
   */
  private def checkReturns(ast: Ast): EndStatus = {
    ast match {

      case Source(defs) =>
        for df <- defs do {
          checkReturns(df)
        }
        EndStatus(Set.empty, false)

      case Block(stats) =>
        var endStatus = EndStatus(Set.empty, false)
        for stat <- stats do {
          if (endStatus.alwaysStopped) {
            reportError("dead code", stat.getPosition)
          } else {
            endStatus = checkReturns(stat)
          }
        }
        endStatus

      case ifThenElse@IfThenElse(_, thenBr, elseBrOpt) =>
        val thenEndStatus = checkReturns(thenBr)
        elseBrOpt match {
          case Some(elseBr) =>
            val elseEndStatus = checkReturns(elseBr)
            if (thenEndStatus.returned.size == 1 && elseEndStatus.returned.size == 1) {
              val thenRet = thenEndStatus.returned.head
              val elseRet = elseEndStatus.returned.head
              if (!thenRet.subtypeOrSupertype(elseRet)) {
                reportError(s"branches of if lead to different return types: '$thenRet', '$elseRet'", ifThenElse.getPosition)
              }
            }
            EndStatus(thenEndStatus.returned ++ elseEndStatus.returned, thenEndStatus.alwaysStopped && elseEndStatus.alwaysStopped)
          case None => thenEndStatus.copy(alwaysStopped = false)
        }

      case _: Ternary => EndStatus(Set.empty, false)

      case whileLoop@WhileLoop(_, body, _) =>
        val bodyEndStatus = checkReturns(body)
        if (bodyEndStatus.alwaysStopped) {
          reportError("while should be replaced by if", whileLoop.getPosition, isWarning = true)
        }
        EndStatus(bodyEndStatus.returned, false)

      case forLoop@ForLoop(_, _, _, body, _) =>
        val bodyEndStatus = checkReturns(body)
        if (bodyEndStatus.alwaysStopped) {
          reportError("for should be replaced by if", forLoop.getPosition, isWarning = true)
        }
        EndStatus(bodyEndStatus.returned, false)

      case retStat: ReturnStat =>
        val retType = retStat.getRetType
        if (retType.isEmpty) {
          reportError("could not infer type of returned value", retStat.getPosition)
        }
        EndStatus(Set(retType.getOrElse(VoidType)), true)

      case _: PanicStat =>
        EndStatus(Set(NothingType), true)

      case _: (FunDef | StructDef | Param) =>
        assert(false)

      case _ => EndStatus(Set.empty, false)

    }
  }

  private def checkVerificationFormulas(formulas: List[(Expr, Option[Position])], ctx: TypeCheckingContext): Unit = {
    for (formulaExpr, pos) <- formulas do {
      val formulaType = check(formulaExpr, ctx)
      if (!formulaType.subtypeOf(BoolType)) {
        reportError(s"formulas in assert and assume statements must have result type '${BoolType.str}', found '$formulaType'", pos)
      }
      if (!isPurelyFunctional(formulaExpr)(ctx.analysisContext)) {
        reportError("verification formulas should only consist of purely functional expressions", pos)
      }
    }
  }

  private def checkControlFlowCond(cond: Expr, ctx: TypeCheckingContext): Unit = {
    val condType = check(cond, ctx)
    if (!condType.subtypeOf(BoolType)){
      reportError(s"control-flow conditions must be of type '$BoolType', found '$condType'", cond.getPosition)
    }
    if (!isPurelyFunctional(cond)(ctx.analysisContext)){
      reportError(s"control-flow conditions are not allowed to use side effects", cond.getPosition)
    }
  }

  private def reportError(msg: String, pos: Option[Position], isWarning: Boolean = false): UndefinedType.type = {
    errorReporter.push(if isWarning then Warning(TypeChecking, msg, pos) else Err(TypeChecking, msg, pos))
    UndefinedType
  }

}
