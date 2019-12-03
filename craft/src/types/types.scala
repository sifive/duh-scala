package duh.scala.types

import duh.scala.{decoders => J}
import org.json4s.JsonAST._

sealed trait DUHType

case class Component(
  name: String,
  ports: Seq[Port],
  pSchema: ParameterSchema) extends DUHType

object Component {
  def fromJSON(json: JValue): J.Result[Component] = {
    J.map3(
      J.field("name", J.string),
      J.field("model",
        J.field("ports", J.objMap(Port.fromJSON(_, _)))),
      J.field("model",
        J.field("pSchema", ParameterSchema.fromJSON)),
      Component(_, _, _))(json)
  }
}

sealed trait InterfaceMode extends DUHType
case object Master extends InterfaceMode
case object Slave extends InterfaceMode
case object Monitor extends InterfaceMode

sealed trait Direction
case object Input extends Direction
case object Output extends Direction
case object Inout extends Direction

object Direction {
  def fromString(string: String): J.Result[Direction] = {
    string match {
      case "in" => J.pass(Input)
      case "out" => J.pass(Output)
      case "inout" => J.pass(Inout)
      case _ => J.fail(s"$string is not a valid direction")
    }
  }
}

trait Literal
trait Expression
case class IntegerLiteral(value: BigInt) extends Expression with Literal
case class Parameter(name: String) extends Expression
case class Negate(expr: Expression) extends Expression

object Expression {
  def fromInteger(integer: BigInt): J.Result[Expression] = {
    J.pass(IntegerLiteral(integer))
  }

  def fromString(string: String): J.Result[Expression] = {
    try {
      J.pass(IntegerLiteral(string.toInt))
    } catch {
      case e: java.lang.NumberFormatException =>
        string.slice(0, 1) match {
          case "" => J.fail("empty string")
          case "-" => J.pass(Negate(Parameter(string.drop(1))))
          case "+" => J.pass(Parameter(string.drop(1)))
          case _ => J.pass(Parameter(string))
        }
    }
  }

  def fromJSON(json: JValue): J.Result[Expression] = {
    J.oneOf(J.string(fromString), J.integer(fromInteger))(json)
  }

  def isInput(expr: Expression): Boolean = expr match {
    case Negate(expr) => !isInput(expr)
    case IntegerLiteral(value) => value >= 0
    case Parameter(_) => true
  }
}

case class Wire(
  width: Expression,
  direction: Direction,
  analogDirection: Option[Direction] = None)

object Wire {
  def fromExpression(expr: Expression): Wire = {
    if (Expression.isInput(expr)) {
      Wire(expr, Input)
    } else {
      Wire(Negate(expr), Output)
    }
  }

  def fromJSON(jvalue: JValue): J.Result[Wire] = {
    J.oneOf(
      Expression.fromJSON(_).map(fromExpression),
      J.map3(
        J.field("width", Expression.fromJSON),
        J.field("direction", J.string(Direction.fromString)),
        J.fieldOption("analog", J.string(Direction.fromString)),
        Wire(_, _, _))
    )(jvalue)
  }
}

case class Port(name: String, wire: Wire)

object Port {
  def fromJSON(name: String, jvalue: JValue): J.Result[Port] = {
    Wire.fromJSON(jvalue).map(w => Port(name, w))
  }

  def fromJSON(json: JValue): J.Result[Port] = {
    J.map2(
      J.field("name", J.string),
      Wire.fromJSON,
      Port(_, _))(json)
  }
}

trait Constraint {
  def satisfies(value: Literal): Boolean
}

case class Minimum(min: BigInt) extends Constraint {
  def satisfies(value: BigInt): Boolean = value >= min

  def satisfies(lit: Literal): Boolean = lit match {
    case IntegerLiteral(value) => satisfies(value)
  }
}

case class ExclusiveMinimum(min: BigInt) extends Constraint {
  def satisfies(value: BigInt): Boolean = value > min

  def satisfies(lit: Literal): Boolean = lit match {
    case IntegerLiteral(value) => satisfies(value)
  }
}

case class Maximum(max: BigInt) extends Constraint {
  def satisfies(value: BigInt): Boolean = value <= max

  def satisfies(lit: Literal): Boolean = lit match {
    case IntegerLiteral(value) => satisfies(value)
  }
}

case class ExclusiveMaximum(max: BigInt) extends Constraint {
  def satisfies(value: BigInt): Boolean = value < max

  def satisfies(lit: Literal): Boolean = lit match {
    case IntegerLiteral(value) => satisfies(value)
  }
}

case class Default(default: BigInt) extends Constraint {
  def satisfies(lit: Literal): Boolean = true
}

object Constraint {
  def fromJSON(name: String, json: JValue): J.Result[Option[Constraint]] = {
    (name, J.integer(json)) match {
      case ("minimum", int) => int.map(i => Some(Minimum(i)))
      case ("exclusiveMinimum", int) => int.map(i => Some(ExclusiveMinimum(i)))
      case ("maximum", int) => int.map(i => Some(Maximum(i)))
      case ("exclusiveMaximum", int) => int.map(i => Some(ExclusiveMaximum(i)))
      case ("default", int) => int.map(i => Some(Default(i)))
      case _ => J.pass(None)
    }
  }
}

case class ParameterSchema(constraints: Map[Parameter, Seq[Constraint]], defaults: Map[Parameter, Literal])

object ParameterSchema {
  def fromJSON(json: JValue): J.Result[ParameterSchema] = {
    J.field("type", J.string {
      case "object" => J.pass(Unit)
      case _ => J.fail("parameter schema must be type 'object'")
    })(json).flatMap(_ => J.field("properties", J.objMap((name, constraints) => {
      J.field("type", J.string({
        case "integer" => J.pass(Unit)
        case _: String => J.fail("only integer parameters are supported")
      }))(constraints).flatMap(_ => J.objMap(Constraint.fromJSON)(constraints))
        .map(Parameter(name) -> _.flatten)
    }))(json).map(constraints => ParameterSchema(constraints.toMap, Map.empty)))
  }
}
