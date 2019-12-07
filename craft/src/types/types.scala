package duh.scala.types

import duh.scala.{decoders => J}
import org.json4s.JsonAST._
import scala.collection.immutable.ListMap
import scala.util.matching.Regex

sealed trait DUHType

case class Component(
  name: String,
  ports: Seq[Port],
  pSchema: ParameterSchema,
  busInterfaces: Seq[BusInterface]) extends DUHType

object Component {
  def fromJSON[T](fn: Component => J.Result[T])(json: JValue): J.Result[T] = {
    J.map4(
      J.field("name", J.string),
      J.field("model",
        J.field("ports", J.oneOf(J.objMap(Port.fromJSON), J.arrMap(Port.fromJSON)))),
      J.field("pSchema", ParameterSchema.fromJSON),
      J.field("busInterfaces", J.arrMap(BusInterface.fromJSON)),
      Component(_, _, _, _))(json).flatMap(fn)
  }

  def fromJSON(json: JValue): J.Result[Component] = fromJSON(J.pass(_: Component))(json)
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
  def fromJSON(json: JValue): J.Result[Direction] = J.string(fromString(_))(json)

  def fromString(string: String): J.Result[Direction] = {
    string match {
      case "in" => J.pass(Input)
      case "out" => J.pass(Output)
      case "inout" => J.pass(Inout)
      case _ => J.fail(s"$string is not a valid direction")
    }
  }
}

sealed trait Expression
case class Parameter(name: String) extends Expression
case class Negate(expr: Expression) extends Expression

sealed trait Literal extends Expression
case class IntegerLiteral(value: BigInt) extends Literal
case class StringLiteral(value: String) extends Literal
case class DoubleLiteral(value: Double) extends Literal

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

  def fromJSON[T](fn: Expression => J.Result[T])(json: JValue): J.Result[T] = {
    J.oneOf(J.string(fromString), J.integer(fromInteger))(json).flatMap(fn)
  }

  def fromJSON(json: JValue): J.Result[Expression] = fromJSON(J.pass(_: Expression))(json)

  def isInput(expr: Expression): Boolean = expr match {
    case Negate(expr) => !isInput(expr)
    case IntegerLiteral(value) => value >= 0
    case Parameter(_) => true
  }

  def evaluate(expr: Expression, pSchema: ParameterSchema, params: JValue): J.Result[Literal] =
    expr match {
      case Parameter(name) => pSchema.getValue(name)(params)
      case Negate(child) => evaluate(child, pSchema, params).flatMap {
        case IntegerLiteral(value) => J.pass(IntegerLiteral(-value))
      }
      case lit: Literal => J.pass(lit)
    }
}

case class Wire(
  width: Expression,
  direction: Direction,
  analogDirection: Option[Direction] = None)

object Wire {
  def fromExpression(expr: Expression): J.Result[Wire] = {
    J.pass(if (Expression.isInput(expr)) {
      Wire(expr, Input)
    } else {
      Wire(Negate(expr), Output)
    })
  }

  def fromJSON[T](fn: Wire => J.Result[T])(json: JValue): J.Result[T] = {
    J.oneOf(
      Expression.fromJSON(fromExpression),
      J.map3(
        J.field("width", Expression.fromJSON),
        J.field("direction", J.string(Direction.fromString)),
        J.fieldOption("analog", J.string(Direction.fromString)),
        Wire(_, _, _))
    )(json).flatMap(fn)
  }

  def fromJSON(json: JValue): J.Result[Wire] = fromJSON(J.pass(_: Wire))(json)
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

sealed trait Constraint[T] {
  def satisfies(value: T): Boolean
}

sealed trait IntegerConstraint extends Constraint[BigInt]

object IntegerConstraint {
  case class Minimum(min: BigInt) extends IntegerConstraint {
    def satisfies(value: BigInt): Boolean = value >= min
  }

  case class ExclusiveMinimum(min: BigInt) extends IntegerConstraint {
    def satisfies(value: BigInt): Boolean = value > min
  }

  case class Maximum(max: BigInt) extends IntegerConstraint {
    def satisfies(value: BigInt): Boolean = value <= max
  }

  case class ExclusiveMaximum(max: BigInt) extends IntegerConstraint {
    def satisfies(value: BigInt): Boolean = value < max
  }

  def fromJSON(name: String, json: JValue): J.Result[Option[IntegerConstraint]] = {
    (name, J.integer(json)) match {
      case ("minimum", int) => int.map(i => Some(Minimum(i)))
      case ("exclusiveMinimum", int) => int.map(i => Some(ExclusiveMinimum(i)))
      case ("maximum", int) => int.map(i => Some(Maximum(i)))
      case ("exclusiveMaximum", int) => int.map(i => Some(ExclusiveMaximum(i)))
      case _ => J.pass(None)
    }
  }

  def collectFromJSON(json: JValue): J.Result[Seq[IntegerConstraint]] = {
    J.hasType("integer",
      J.objMap(fromJSON))(json).map { constraints => constraints.flatten
    }
  }
}

sealed trait DoubleConstraint extends Constraint[Double]

object DoubleConstraint {
  case class Minimum(min: Double) extends DoubleConstraint {
    def satisfies(value: Double): Boolean = value >= min
  }

  case class ExclusiveMinimum(min: Double) extends DoubleConstraint {
    def satisfies(value: Double): Boolean = value > min
  }

  case class Maximum(max: Double) extends DoubleConstraint {
    def satisfies(value: Double): Boolean = value <= max
  }

  case class ExclusiveMaximum(max: Double) extends DoubleConstraint {
    def satisfies(value: Double): Boolean = value < max
  }

  def fromJSON(name: String, json: JValue): J.Result[Option[DoubleConstraint]] = {
    (name, J.double(json)) match {
      case ("minimum", double) => double.map(d => Some(Minimum(d)))
      case ("exclusiveMinimum", double) => double.map(d => Some(ExclusiveMinimum(d)))
      case ("maximum", double) => double.map(d => Some(Maximum(d)))
      case ("exclusiveMaximum", double) => double.map(d => Some(ExclusiveMaximum(d)))
      case _ => J.pass(None)
    }
  }

  def collectFromJSON(json: JValue): J.Result[Seq[DoubleConstraint]] = {
    J.hasType("number",
      J.objMap(fromJSON))(json).map { constraints => constraints.flatten
    }
  }
}

sealed trait StringConstraint extends Constraint[String]

object StringConstraint {
  case class MinLength(min: BigInt) extends StringConstraint {
    def satisfies(value: String): Boolean = value.length >= min
  }

  case class MaxLength(max: BigInt) extends StringConstraint {
    def satisfies(value: String): Boolean = value.length <= max
  }

  case class Pattern(regex: Regex) extends StringConstraint {
    def satisfies(value: String): Boolean = value match {
      case regex(_*) => true
      case _ => false
    }
  }

  def fromJSON(name: String, json: JValue): J.Result[Option[StringConstraint]] = {
    (name, json) match {
      case ("minLength", length) => J.integer(length).map(s => Some(MinLength(s)))
      case ("maxLength", length) => J.integer(length).map(s => Some(MaxLength(s)))
      case ("pattern", regex) => J.string(regex).map(s => Some(Pattern(s.r)))
      case _ => J.pass(None)
    }
  }

  def collectFromJSON(json: JValue): J.Result[Seq[StringConstraint]] = {
    J.hasType("string",
      J.objMap(fromJSON))(json).map { constraints => constraints.flatten
    }
  }
}

sealed trait ParameterDefinition {
  type ParamType

  def name: String
  def default: Option[ParamType]
  def constraints: Seq[Constraint[ParamType]]

  protected def fromJSONImp(json: JValue): J.Result[ParamType]
  
  final def fromJSON(json: JValue): J.Result[ParamType] = {
    fromJSONImp(json).flatMap {
      case value if constraints.forall(_.satisfies(value)) => J.pass(value)
      case value => J.fail(s"$value does not satisfy constraints")
    }
  }
}

object ParameterDefinition {
  def fromJSON(name: String, json: JValue): J.Result[ParameterDefinition] = {
    J.oneOf(
      IntegerParameter.fromJSON(name, _),
      DoubleParameter.fromJSON(name, _),
      StringParameter.fromJSON(name, _)
    )(json)
  }
}

case class IntegerParameter(
  name: String,
  default: Option[BigInt],
  constraints: Seq[IntegerConstraint]) extends ParameterDefinition {
  type ParamType = BigInt
  def fromJSONImp(json: JValue) = J.fieldOption(name, J.integer)(json).flatMap {
    case Some(value) => J.pass(value)
    case None => default match {
      case Some(value) => J.pass(value)
      case None => J.fail(s"no parameter named '$name'")
    }
  }
}

object IntegerParameter {
  def fromJSON(name: String, json: JValue): J.Result[IntegerParameter] = {
    J.map2(
      J.fieldOption("default", J.integer),
      IntegerConstraint.collectFromJSON,
      IntegerParameter(name, _, _)
    )(json)
  }
}

case class DoubleParameter(
  name: String,
  default: Option[Double],
  constraints: Seq[DoubleConstraint]) extends ParameterDefinition {
  type ParamType = Double
  def fromJSONImp(json: JValue) = J.double(json)
}

object DoubleParameter {
  def fromJSON(name: String, json: JValue): J.Result[DoubleParameter] = {
    J.map2(
      J.fieldOption("default", J.double),
      DoubleConstraint.collectFromJSON,
      DoubleParameter(name, _, _)
    )(json)
  }
}

case class StringParameter(
  name: String,
  default: Option[String],
  constraints: Seq[StringConstraint]) extends ParameterDefinition {
  type ParamType = String
  def fromJSONImp(json: JValue) = J.string(json)
}

object StringParameter {
  def fromJSON(name: String, json: JValue): J.Result[StringParameter] = {
    J.map2(
      J.fieldOption("default", J.string),
      StringConstraint.collectFromJSON,
      StringParameter(name, _, _)
    )(json)
  }
}

case class ParameterSchema(definitions: Seq[ParameterDefinition]) {
  private val nameMap = definitions.map(p => p.name -> p).toMap.lift

  def getValue(name: String)(json: JValue): J.Result[Literal] = {
    J.oneOf(
      getIntegerValue(name)(_).map(IntegerLiteral),
    )(json)
  }

  def getIntegerValue(name: String)(json: JValue): J.Result[BigInt] = {
    nameMap(name) match {
      case Some(paramDef: IntegerParameter) => paramDef.fromJSON(json)
      case Some(paramDef) => J.fail(s"parameter '$name' is not defined as an integer in parameter schema")
      case None => J.fail(s"no parameter named '$name' defined in parameter schema")
    }
  }

  def getDoubleValue(name: String)(json: JValue): J.Result[Double] = {
    nameMap(name) match {
      case Some(paramDef: DoubleParameter) => paramDef.fromJSON(json)
      case Some(paramDef) => J.fail(s"parameter '$name' is not defined as a double in parameter schema")
      case None => J.fail(s"no parameter named '$name' defined in parameter schema")
    }
  }

  def getStringValue(name: String)(json: JValue): J.Result[String] = {
    nameMap(name) match {
      case Some(paramDef: StringParameter) => paramDef.fromJSON(json)
      case Some(paramDef) => J.fail(s"parameter '$name' is not defined as a string in parameter schema")
      case None => J.fail(s"no parameter named '$name' defined in parameter schema")
    }
  }
}

object ParameterSchema {
  def fromJSON(json: JValue): J.Result[ParameterSchema] = {
    J.hasType("object",
      J.field("properties", J.objMap(ParameterDefinition.fromJSON)))(json)
        .map(ParameterSchema(_))
  }
}

case class BusInterface(
  busType: BusInterfaceType,
  abstractionTypes: Seq[BusAbstractionType]
)

trait BusInterfaceType
case class StandardBusInterface(
  vendor: String,
  library: String,
  name: String,
  version: String) extends BusInterfaceType
case class NonStandardBusInterface(name: String) extends BusInterfaceType

object StandardBusInterface {
  def fromJSON(json: JValue): J.Result[StandardBusInterface] = {
    J.map4(
      J.field("vendor", J.string),
      J.field("library", J.string),
      J.field("name", J.string),
      J.field("version", J.string),
      StandardBusInterface(_, _, _, _))(json)
  }
}


object BusInterfaceType {
  def fromJSON(json: JValue): J.Result[BusInterfaceType] = {
    J.oneOf(
      J.string(n => J.pass(NonStandardBusInterface(n))),
      StandardBusInterface.fromJSON
    )(json)
  }
}

sealed trait PortMaps
case class ArrayPortMaps(ports: Seq[String]) extends PortMaps
case class FieldPortMaps(map: ListMap[String, String]) extends PortMaps

object PortMaps {
  def fromJSON(json: JValue): J.Result[PortMaps] = {
    J.oneOf(
      J.objMap((fieldName, portName) => J.string(portName).map(fieldName -> _))(_)
        .map(pairs => FieldPortMaps(ListMap(pairs:_*))),
      J.arrMap(J.string)(_).map(ArrayPortMaps(_))
    )(json)
  }
}

case class BusAbstractionType(viewRef: Option[String], portMaps: Option[PortMaps])

object BusAbstractionType {
  def fromJSON(json: JValue): J.Result[BusAbstractionType] = {
    J.map2(
      J.fieldOption("viewRef", J.string),
      J.fieldOption("portMaps", PortMaps.fromJSON),
      (viewRef: Option[String], portMapsOpt: Option[PortMaps]) =>
        BusAbstractionType(viewRef, portMapsOpt)
    )(json)
  }
}

object BusInterface {
  def fromJSON(json: JValue): J.Result[BusInterface] = {
    J.map2(
      J.field("busType", BusInterfaceType.fromJSON),
      J.fieldOption("abstractionTypes", J.arrMap(BusAbstractionType.fromJSON)),
      (busType: BusInterfaceType, abstractionTypesOpt: Option[Seq[BusAbstractionType]]) =>
        BusInterface(busType, abstractionTypesOpt.getOrElse(Seq.empty))
    )(json)
  }
}

case class BusPortDefinition(
  name: String,
  description: Option[String],
  onMaster: BusWireDefinition,
  onSlave: BusWireDefinition,
  defaultValue: Option[BigInt],
  isClock: Boolean,
  requiresDriver: Boolean
)

trait BusPresence
case object Optional extends BusPresence
case object Required extends BusPresence
case class BusWireDefinition(presence: BusPresence, width: Expression, direction: Direction)

object BusWireDefinition {
  def fromJSON(json: JValue): J.Result[BusWireDefinition] = {
    J.map3(
      J.field("presence", J.string {
        case "optional" => J.pass(Optional)
        case "required" => J.pass(Required)
        case other => J.fail(s"'$other' is not a valid bus wire presence value")
      }),
      J.fieldOption("width", Expression.fromJSON)(_)
        .map(_.getOrElse(IntegerLiteral(BigInt(1)))),
      J.field("direction", Direction.fromJSON),
      BusWireDefinition(_, _, _)
    )(json)
  }
}

object BusPortDefinition {
  case class WirePair(onMaster: BusWireDefinition, onSlave: BusWireDefinition)

  def fromJSON(name: String, json: JValue): J.Result[BusPortDefinition] = {
    J.map5(
      J.fieldOption("description", J.string),
      J.field("wire", J.map2(
        J.field("onMaster", BusWireDefinition.fromJSON),
        J.field("onSlave", BusWireDefinition.fromJSON),
        WirePair(_, _)
      )),
      J.fieldOption("defaultValue", J.integer),
      J.fieldOption("isClock", J.boolean)(_).map(_.getOrElse(false)),
      J.fieldOption("requiresDriver", J.boolean)(_).map(_.getOrElse(false)),
      (desc: Option[String], wire: WirePair, default: Option[BigInt], isClock: Boolean, requiresDriver: Boolean) =>
        BusPortDefinition(
          name,
          desc,
          wire.onMaster,
          wire.onSlave,
          default,
          isClock,
          requiresDriver)
    )(json)
  }
}

case class BusDefinition(
  busType: StandardBusInterface,
  description: Option[String],
  ports: Seq[BusPortDefinition])

object BusDefinition {
  def fromJSON(json: JValue): J.Result[BusDefinition] = {
    J.map3(
      StandardBusInterface.fromJSON,
      J.fieldOption("description", J.string),
      J.field("ports", J.objMap(BusPortDefinition.fromJSON)),
      BusDefinition(_, _, _)
    )(json)
  }
}
