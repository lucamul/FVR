package compiler.irs

import compiler.Position
import lang.Types.*
import lang.Types.PrimitiveType.*
import lang.{FunctionSignature, Keyword, Operator}
import Keyword.*

object Asts {

  sealed abstract class Ast {
    // Positions are propagated by the TreeParser
    // Each AST is assigned the position of its leftmost token (by the map method of TreeParser)
    private var positionOpt: Option[Position] = None

    def setPosition(posOpt: Option[Position]): Unit = {
      positionOpt = posOpt
    }

    final def setPosition(pos: Position): Unit = {
      setPosition(Some(pos))
    }

    final def getPosition: Option[Position] = positionOpt

    def children: List[Ast]

    final def collect[T](pf: PartialFunction[Ast, T]): List[T] = {
      val thisRes = if pf.isDefinedAt(this) then Some(pf(this)) else None
      val childrenRes = children.flatMap(_.collect(pf))
      (thisRes ++ childrenRes).toList
    }

    final def collectLs[T](pf: PartialFunction[Ast, List[T]]): List[T] = {
      val thisRes = if pf.isDefinedAt(this) then pf(this) else Nil
      val childrenRes = children.flatMap(_.collectLs(pf))
      thisRes ++ childrenRes
    }

    /**
     * Crashes the compiler if at least 1 expression in the AST has no type (i.e. `expr.getTypeOpt.isEmpty`)
     */
    final def assertAllTypesAreSet(): Unit = {
      collect {
        case expr: Expr => assert(expr.getTypeOpt.isDefined, s"no type set for $expr")
        case _ => ()
      }
    }
  }

  sealed abstract class Statement extends Ast

  sealed abstract class Expr extends Statement {
    private var tpeOpt: Option[Type] = None

    /**
     * Set the type that has been inferred for this expression
     *
     * WARNING: since subclasses are allowed to overrid [[getTypeOpt]], setting a type might have no effect
     */
    def setTypeOpt(tpe: Option[Type]): Expr = {
      tpeOpt = tpe
      this
    }

    /**
     * See [[setTypeOpt]]
     */
    final def setType(tpe: Type): Expr = {
      setTypeOpt(Some(tpe))
    }

    /**
     * @return the type that has been inferred for this expression
     *
     *         WARNING: might be overriden by subclasses if they are able to infer their type from the types of their children
     */
    def getTypeOpt: Option[Type] = tpeOpt

    /**
     * See [[getTypeOpt]]
     */
    final def getType: Type = {
      getTypeOpt match
        case Some(tpe) => tpe
        case None => throw new NoSuchElementException(s"type missing in $this")
    }

  }

  /**
   * Code source (most of the time a file)
   */
  final case class Source(defs: List[TopLevelDef]) extends Ast {
    private var name: String = "<missing name>"

    override def children: List[Ast] = defs

    def setName(name: String): Source = {
      this.name = name
      this
    }

    def getName: String = name
  }

  /**
   * Block (scope):
   * {{{
   *   {
   *    ...
   *   }
   * }}}
   */
  final case class Block(stats: List[Statement]) extends Statement {
    override def children: List[Ast] = stats
  }

  sealed abstract class TopLevelDef extends Ast

  /**
   * Function definition
   */
  final case class FunDef(
                           funName: String,
                           params: List[Param],
                           optRetType: Option[Type],
                           body: Block,
                           precond: List[Expr],
                           postcond: List[Expr],
                           verifIgnore: Boolean
                         ) extends TopLevelDef {
    val signature: FunctionSignature = FunctionSignature(funName, params.map(_.tpe), optRetType.getOrElse(VoidType))

    override def children: List[Ast] = params ++ List(body) ++ precond ++ postcond
  }

  /**
   * Structure (`struct`) definition
   */
  final case class StructDef(structName: String, fields: List[Param]) extends TopLevelDef {
    override def children: List[Ast] = fields
  }

  /**
   * Parameter of a function
   */
  final case class Param(paramName: String, tpe: Type) extends Ast {
    override def children: List[Ast] = Nil
  }

  /**
   * `val` or `var` definition
   *
   * @param optType        type, if explicitely given
   * @param rhs            expression producing the value that will be assigned to the local
   * @param isReassignable `true` if `var`, `false` if `val`
   */
  final case class LocalDef(localName: String, var optType: Option[Type], rhs: Expr, isReassignable: Boolean) extends Statement {
    val keyword: Keyword = if isReassignable then Keyword.Var else Keyword.Val

    override def children: List[Ast] = List(rhs)
  }

  sealed abstract class Literal extends Expr {
    val value: Any

    final override def children: List[Ast] = Nil
  }

  /**
   * Integer literal
   */
  final case class IntLit(value: Int) extends Literal {
    override def getTypeOpt: Option[Type] = Some(IntType)
  }

  /**
   * Double literal
   */
  final case class DoubleLit(value: Double) extends Literal {
    override def getTypeOpt: Option[Type] = Some(DoubleType)
  }

  /**
   * Char literal
   */
  final case class CharLit(value: Char) extends Literal {
    override def getTypeOpt: Option[Type] = Some(CharType)
  }

  /**
   * Bool (boolean) literal
   */
  final case class BoolLit(value: Boolean) extends Literal {
    override def getTypeOpt: Option[Type] = Some(BoolType)
  }

  /**
   * String literal
   */
  final case class StringLit(value: String) extends Literal {
    override def getTypeOpt: Option[Type] = Some(StringType)
  }

  /**
   * Occurence of a variable (`val`, `var`, function parameter, etc.)
   */
  final case class VariableRef(name: String) extends Expr {
    
    override def children: List[Ast] = Nil
  }

  /**
   * Function call: `callee(args)`
   */
  final case class Call(callee: String, args: List[Expr]) extends Expr {
    
    override def children: List[Ast] = args
  }

  /**
   * Array indexing: `indexed[arg]`
   */
  final case class Indexing(indexed: Expr, arg: Expr) extends Expr {
    
    override def children: List[Ast] = List(indexed, arg)

    def setPositionSp(posOpt: Option[Position]): Indexing = {
      setPosition(posOpt)
      this
    }
  }

  /**
   * Initialization of an (empty) array
   */
  final case class ArrayInit(elemType: Type, size: Expr) extends Expr {
    
    override def children: List[Ast] = List(size)
  }

  /**
   * Initialization of an array that contains all the elements in `arrayElems` (in order)
   */
  final case class FilledArrayInit(arrayElems: List[Expr]) extends Expr {
    
    override def children: List[Ast] = arrayElems
  }

  /**
   * Initialization of a struct, e.g. `new Foo { 0, 1 }`
   */
  final case class StructInit(structName: String, args: List[Expr]) extends Expr {
    
    override def children: List[Ast] = args
  }

  /**
   * Unary operator
   */
  final case class UnaryOp(operator: Operator, operand: Expr) extends Expr {
    
    override def children: List[Ast] = List(operand)
  }

  /**
   * Binary operator
   */
  final case class BinaryOp(lhs: Expr, operator: Operator, rhs: Expr) extends Expr {
    
    override def children: List[Ast] = List(lhs, rhs)
  }

  /**
   * Access to a struct field: `lhs.select`
   */
  final case class Select(lhs: Expr, selected: String) extends Expr {
    
    override def children: List[Ast] = List(lhs)
  }

  sealed abstract class Assignment extends Statement

  /**
   * Assignment of a value to a variable (or struct field, or in an array): `lhs = rhs`
   */
  final case class VarAssig(lhs: Expr, rhs: Expr) extends Assignment {
    override def children: List[Ast] = List(lhs, rhs)
  }

  /**
   * In-place mutation of a variable (or struct field, or in an array), e.g. `x += 1`
   *
   * @param op the DESUGARED operator, e.g. `+` in a `+=` expression
   */
  final case class VarModif(lhs: Expr, rhs: Expr, op: Operator) extends Assignment {
    override def children: List[Ast] = List(lhs, rhs)
  }

  /**
   * If-then-else:
   * {{{
   *   if cond {
   *     thenBr
   *   } else {
   *     elseBr
   *   }
   * }}}
   */
  final case class IfThenElse(cond: Expr, thenBr: Statement, elseBrOpt: Option[Statement]) extends Statement {
    override def children: List[Ast] = List(cond, thenBr) ++ elseBrOpt
  }

  /**
   * Ternary operator:
   * {{{
   *   when cond then thenBr else elseBr
   * }}}
   */
  final case class Ternary(cond: Expr, thenBr: Expr, elseBr: Expr) extends Expr {
    
    override def children: List[Ast] = List(cond, thenBr, elseBr)
  }

  /**
   * While loop:
   * {{{
   *   while cond {
   *     body stats
   *   }
   * }}}
   */
  final case class WhileLoop(cond: Expr, body: Statement, invariants: List[Expr]) extends Statement {
    override def children: List[Ast] = List(cond, body) ++ invariants
  }

  /**
   * For loop:
   * {{{
   *   for initStats; cond; stepStats {
   *     body stats
   *   }
   * }}}
   */
  final case class ForLoop(
                            initStats: List[LocalDef | Assignment],
                            cond: Expr,
                            stepStats: List[Assignment],
                            body: Block,
                            invariants: List[Expr]
                          ) extends Statement {
    override def children: List[Ast] = initStats ++ List(cond) ++ stepStats ++ List(body) ++ invariants
  }

  /**
   * `return` statement, with or without return value
   */
  final case class ReturnStat(optVal: Option[Expr]) extends Statement {
    def getRetType: Option[Type] = if optVal.isDefined then optVal.get.getTypeOpt else Some(VoidType)

    override def children: List[Ast] = optVal.toList
  }

  /**
   * Cast, e.g. `x as Int`
   */
  final case class Cast(expr: Expr, tpe: Type) extends Expr {
    
    override def children: List[Ast] = List(expr)
  }

  /**
   * `panic` statement
   */
  final case class PanicStat(msg: Expr) extends Statement {
    override def children: List[Ast] = List(msg)
  }

  /**
   * Node generated by desugaring
   *
   * @param stats statements to be executed before [[expr]]
   * @param exprOpt  the expression to be executed after [[stats]]. Its return value will be the return value of the
   *                 whole Sequence (return type is `Void` if no expression)
   */
  final case class Sequence(stats: List[Statement], exprOpt: Option[Expr]) extends Expr {
    
    override def children: List[Ast] = stats ++ exprOpt

    override def getTypeOpt: Option[Type] = {
      exprOpt match
        case Some(expr) =>
          expr.getTypeOpt
        case None =>
          Some(VoidType)
    }
  }

  /**
   * Assertion or assumption
   * @param formulaExpr the asserted/assumed formula
   * @param _descr description
   * @param isAssumed whether this formula must be proven (assertion) or can be assumed (assumption)
   *
   * The `_descr` member of this case class is the initial description, while the `description` method returns
   * the whole description (including the description of the position if it has been set)
   */
  final case class Assertion(formulaExpr: Expr, private val _descr: String, isAssumed: Boolean) extends Statement {
    private var posDescr = ""

    /*
     * I know this design with the descriptions is not intuitive but it is the most robust I found to avoid
     * duplicating positions in the description (because positions might be set more than once, and it is
     * hard to make sure that this does not happen)
     */

    def descr: String = _descr ++ posDescr

    override def children: List[Ast] = List(formulaExpr)

    override def setPosition(posOpt: Option[Position]): Unit = {
      super.setPosition(posOpt)
      posOpt.foreach { pos =>
        if (posDescr.isEmpty){
          posDescr += s" at ${pos.toStringSimpleFilename}"
        }
      }
    }

    /**
     * Specialized version of `setPosition`: set the position and return this
     */
    def setPositionSp(posOpt: Option[Position]): Assertion = {
      setPosition(posOpt)
      this
    }

    def keyword: Keyword = if isAssumed then Assume else Assert
  }

}
