package duh.scala.types

import fastparse._
import fastparse.Parsed.{Success, Failure}
import SingleLineWhitespace._

import duh.{decoders => J}
import org.json4s.JsonAST._

sealed trait Expression
object Expression {
  sealed trait Literal extends Expression
  case class IntLit(i: BigInt) extends Literal
  case class DecLit(i: BigDecimal) extends Literal
  case class BoolLit(b: Boolean) extends Literal
  case class StrLit(s: String) extends Literal
  object Literal {
    def parseIntLit[_: P]: P[IntLit] = P(long | integer)

    def long[_: P]: P[IntLit] = P(integer ~ ("l" | "L"))
    def integer[_: P]: P[IntLit] = P(("+" | "-").?.! ~ (dec | oct | hex | bin)).map {
      case ("-", i) => IntLit(-i)
      case (_, i) => IntLit(i)
    }

    def dec[_: P] = P(CharIn("1-9") ~ CharIn("0-9").rep | "0").!.map(scala.BigInt(_))
    def oct[_: P] = P("0" ~ ("o" | "O") ~ CharIn("0-7").rep(1).!).map(scala.BigInt(_, 8))
    def hex[_: P] = P("0" ~ ("x" | "X") ~ CharIn("a-f", "A-F", "0-9").rep(1).!).map(scala.BigInt(_, 16))
    def bin[_: P] = P("0" ~ ("b" | "B") ~ ("0" | "1").rep(1).!).map(scala.BigInt(_, 2))
  }

  // accessor into params JSON
  sealed trait Accessor extends Expression
  case class Identifier(value: String) extends Accessor
  case class Field(value: Accessor, field: Identifier) extends Accessor
  case class Index(value: Accessor, index: Expression) extends Accessor
  object Accessor {
    def letter[_: P] = P(CharIn("a-z") | CharIn("A-Z"))
    def digit[_: P] = P(CharIn("0-9"))
    def base[_: P] = P((letter | "_") ~ (letter | digit | "_").rep).!.map(Identifier(_))

    def parseID[_: P]: P[Identifier] = base

    def parseAccessor[_: P]: P[Accessor] = P(base ~ (field | index).rep).map {
      case (id, fns) => fns.foldLeft(id: Accessor) { case (expr, fn) => fn(expr) }
    }
    def field[_: P]: P[Accessor => Accessor] = P("." ~ base).map {
      case field => Field(_, field)
    }
    def index[_: P]: P[Accessor => Accessor] = P("[" ~ parseExpr ~ "]").map {
      case idx => Index(_, idx)
    }
  }

  sealed abstract class Symbol(value: String) {
    private[Expression] def p[_: P]: P[this.type] = P(value).map(_ => this)
  }

  sealed abstract class UnaryOp(value: String) extends Symbol(value)
  object UnaryOp {
    case object Negate extends UnaryOp("-")
    case object BitNot extends UnaryOp("~")
    case object BoolNot extends UnaryOp("!")
    //"!" "~" "&" "~&" "|" "~|" "^" "~^" "^~" "++" "--"

    def parseOp[_: P] = P(Negate.p | BitNot.p | BoolNot.p)
  }
  case class UnaryOpExpr(op: UnaryOp, operand: Expression) extends Expression

  sealed abstract class BinOp(value: String) extends Symbol(value)
  case object BinOp {
    case object Add extends BinOp("+")
    case object Sub  extends BinOp("-")
    case object Mult  extends BinOp("*")
    case object Div  extends BinOp("/")
    case object Mod  extends BinOp("%")
    case object Pow  extends BinOp("**")
    case object LShift  extends BinOp("<<")
    case object RShift  extends BinOp(">>")
    case object BitOr  extends BinOp("|")
    case object BitXor  extends BinOp("^")
    case object BitAnd  extends BinOp("&")
  }
  case class BinOpExpr(left: Expression, op: BinOp, right: Expression) extends Expression

  sealed abstract class CompOp(value: String) extends BinOp(value)
  object CompOp {
    case object Eq extends CompOp("==")
    case object NotEq extends CompOp("!=")
    case object Lt extends CompOp("<")
    case object LtE extends CompOp("<=")
    case object Gt extends CompOp(">")
    case object GtE extends CompOp(">=")

    def parseEqOps[_: P] = P(Eq.p | NotEq.p)
    def parseAngleOps[_: P] = P(Lt.p | LtE.p | Gt.p | GtE.p)
  }

  sealed abstract class BooleanOp(value: String) extends BinOp(value)
  object BooleanOp {
    case object And extends BooleanOp("&&")
    case object Or extends BooleanOp("||")
  }
  case class IfExpr(test: Expression, body: Expression, orelse: Expression) extends Expression

  private def chain[_: P](p: => P[Expression], op: => P[BinOp]) = P(p ~ (op ~ p).rep).map {
    case (lhs, chunks) =>
      chunks.foldLeft(lhs) {
        case (lhs, (op, rhs)) => BinOpExpr(lhs, op, rhs)
      }
  }

  private def baseExpr[_: P] = P(Accessor.parseAccessor | Literal.parseIntLit | "(" ~ parseExpr ~ ")")
  private def unaryExpr[_: P]: P[Expression] = P(baseExpr | (UnaryOp.parseOp ~ unaryExpr).map {
    case (op, operand) => UnaryOpExpr(op, operand)
  })
  private def powerExpr[_: P] = P(chain(unaryExpr, BinOp.Pow.p))
  private def mulDivExpr[_: P] = P(chain(powerExpr, BinOp.Mult.p | BinOp.Div.p | BinOp.Mod.p ))
  private def addSubExpr[_: P] = P(chain(mulDivExpr, BinOp.Add.p | BinOp.Sub.p))
  private def shiftExpr[_: P] = P(chain(addSubExpr, BinOp.LShift.p | BinOp.RShift.p))
  private def angleExpr[_: P] = P(chain(shiftExpr, CompOp.parseAngleOps))
  private def eqExpr[_: P] = P(chain(angleExpr, CompOp.parseEqOps))
  private def bitAndExpr[_: P] = P(chain(eqExpr, BinOp.BitAnd.p))
  private def bitXorExpr[_: P] = P(chain(bitAndExpr, BinOp.BitXor.p))
  private def bitOrExpr[_: P] = P(chain(bitXorExpr, BinOp.BitOr.p))
  private def andExpr[_: P] = P(chain(bitOrExpr, BooleanOp.And.p))
  private def orExpr[_: P] = P(chain(andExpr, BooleanOp.Or.p))
  private def ifExpr[_: P] = P((orExpr ~ "?" ~ orExpr ~ ":").? ~ orExpr).map {
    case (Some((cond, thenExpr)), elseExpr) => IfExpr(cond, thenExpr, elseExpr)
    case (None, expr) => expr
  }

  def parseExpr[_: P]: P[Expression] = ifExpr

  val fromString: String => J.Result[Expression] = str => parse(str, parseExpr(_)) match {
    case Success(value, _) => J.pass(value)
    case f: Failure => J.fail("parse error: " + f.trace().label)
  }
  val fromInteger: BigInt => J.Result[Expression] = i => J.pass(IntLit(i))
  val fromJSON: J.Decoder[Expression] = J.oneOf(J.string(fromString), J.integer(fromInteger))
  val fromBoolean: Boolean => J.Result[Expression] = b => J.pass(BoolLit(b))

  private def expandAccessor(acc: Accessor, tail: Seq[Accessor] = Seq.empty): Seq[Accessor] = acc match {
    case id: Identifier => id +: tail.reverse
    case f: Field => expandAccessor(acc, f +: tail)
    case i: Index => expandAccessor(acc, i +: tail)
  }

  def evaluateAccessor(acc: Accessor, params: JValue): J.Result[Literal] = {
    val expanded: Seq[Accessor] = expandAccessor(acc)
    var decoder: J.Decoder[Expression] = fromJSON
    for (acc <- expanded) {
      acc match {
        case Identifier(id) =>
          decoder = J.field(id, decoder)
        case Field(_, id) =>
          decoder = J.field(id.value, decoder)
        case Index(_, expr) => evaluate(expr, params).map {
          case IntLit(i) =>
            decoder = J.index(i.toInt, decoder)
          case _ =>
            J.fail("type error: accessor index must be an Integer type")
        }
      }
    }
    decoder(params).flatMap(evaluate(_, params))
  }

  def evaluate(expr: Expression, params: JValue): J.Result[Literal] = expr match {
    case lit: Literal => J.pass(lit)
    case acc: Accessor => evaluateAccessor(acc, params)
    case UnaryOpExpr(op, operand) =>
      evaluate(operand, params).flatMap { rec =>
        (op, rec) match {
          case (UnaryOp.Negate, IntLit(b)) => J.pass(IntLit(-b))
          case (UnaryOp.BitNot, IntLit(b)) => J.pass(IntLit(~b))
          case (UnaryOp.BoolNot, BoolLit(b)) => J.pass(BoolLit(!b))
          case _ => J.fail("type error")
        }
      }
    case BinOpExpr(left, op, right) =>
      evaluate(left, params).flatMap(l => evaluate(right, params).map(l -> _)).flatMap { case (l, r) =>
        (op, l, r) match {
          case (BinOp.Add, IntLit(li), IntLit(ri)) => J.pass(IntLit(li + ri))
          case (BinOp.Add, DecLit(li), DecLit(ri)) => J.pass(DecLit(li + ri))

          case (BinOp.Sub, IntLit(li), IntLit(ri)) => J.pass(IntLit(li - ri))
          case (BinOp.Sub, DecLit(li), DecLit(ri)) => J.pass(DecLit(li - ri))

          case (BinOp.Mult, IntLit(li), IntLit(ri)) => J.pass(IntLit(li * ri))
          case (BinOp.Mult, DecLit(li), DecLit(ri)) => J.pass(DecLit(li * ri))

          case (BinOp.Div, IntLit(li), IntLit(ri)) => J.pass(IntLit(li / ri))
          case (BinOp.Div, DecLit(li), DecLit(ri)) => J.pass(DecLit(li / ri))

          case (BinOp.Mod, IntLit(li), IntLit(ri)) => J.pass(IntLit(li % ri))
          case (BinOp.Pow, IntLit(li), IntLit(ri)) => J.pass(IntLit(li.pow(ri.toInt)))
          case (BinOp.Pow, DecLit(li), DecLit(ri)) => J.pass(DecLit(li.pow(ri.toInt)))
          case (BinOp.LShift, IntLit(li), IntLit(ri)) => J.pass(IntLit(li << ri.toInt))
          case (BinOp.RShift, IntLit(li), IntLit(ri)) => J.pass(IntLit(li >> ri.toInt))
          case (BinOp.BitOr, IntLit(li), IntLit(ri)) => J.pass(IntLit(li | ri))
          case (BinOp.BitXor, IntLit(li), IntLit(ri)) => J.pass(IntLit(li ^ ri))
          case (BinOp.BitAnd, IntLit(li), IntLit(ri)) => J.pass(IntLit(li & ri))

          case (CompOp.Eq, op1, op2) => J.pass(BoolLit(op1 == op2))
          case (CompOp.NotEq, op1, op2) => J.pass(BoolLit(op1 != op2))
          case (CompOp.Lt, IntLit(li), IntLit(ri)) => J.pass(BoolLit(li < ri))
          case (CompOp.LtE, IntLit(li), IntLit(ri)) => J.pass(BoolLit(li <= ri))
          case (CompOp.Gt, IntLit(li), IntLit(ri)) => J.pass(BoolLit(li > ri))
          case (CompOp.GtE, IntLit(li), IntLit(ri)) => J.pass(BoolLit(li >= ri))
          case _ => J.fail("type error")
        }
    }
    case IfExpr(test, body, orelse) => evaluate(test, params).flatMap {
      case BoolLit(b) if b => evaluate(body, params)
      case BoolLit(b) => evaluate(orelse, params)
      case _ => J.fail("type error: if expresion condition must be a Boolean type")
    }
  }
}
