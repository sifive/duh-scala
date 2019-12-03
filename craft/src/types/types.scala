package duh.scala.types

import duh.scala.{decoders => J}

sealed trait DUHType

case class Component(
  name: String,
  ports: Seq[Port]) extends DUHType

object Component {
  def fromJSON(json: J.JValue): J.Result[Component] = {
    J.map2(
      J.field("name", J.string),
      J.field("model",
        J.field("ports", J.objMap(Port.fromJSON(_, _)))),
      Component(_, _))(json)
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

trait Expression
case class IntegerLiteral(value: Int) extends Expression
case class Parameter(name: String) extends Expression
case class Negate(expr: Expression) extends Expression

object Expression {
  def fromDouble(double: Double): J.Result[Expression] = {
    J.pass(IntegerLiteral(double.toInt))
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

  def fromJSON(json: J.JValue): J.Result[Expression] = {
    J.oneOf(J.string(fromString), J.number(fromDouble))(json)
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

  def fromJSON(jvalue: ujson.Value): J.Result[Wire] = {
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
  def fromJSON(name: String, jvalue: ujson.Value): J.Result[Port] = {
    Wire.fromJSON(jvalue).map(w => Port(name, w))
  }

  def fromJSON(json: J.JValue): J.Result[Port] = {
    J.map2(
      J.field("name", J.string),
      Wire.fromJSON,
      Port(_, _))(json)
  }
}
