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
  val fromJSON: JValue => J.Result[Component] =
    J.jmapNamed(
      name = J.field("name", J.string),
      ports = J.field("model",
        J.field("ports", J.oneOf(J.objMap(Port.fromJSONField), J.arrMap(Port.fromJSON)))),
      pSchema = J.field("pSchema", ParameterSchema.fromJSON),
      busInterfaces = J.field("busInterfaces", J.arrMap(BusInterface.fromJSON)),
      Component)
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
  val fromJSON: JValue => J.Result[Direction] = J.string(fromString(_))

  val fromString: String => J.Result[Direction] = {
    case "in" => J.pass(Input)
    case "out" => J.pass(Output)
    case "inout" => J.pass(Inout)
    case string => J.fail(s"$string is not a valid direction")
  }
}

sealed trait Expression
case class Parameter(name: String) extends Expression
case class Negate(expr: Expression) extends Expression

sealed trait Literal extends Expression
case class IntegerLiteral(value: BigInt) extends Literal

object Expression {
  val fromInteger: BigInt => J.Result[Expression] = integer => {
    J.pass(IntegerLiteral(integer))
  }

  val fromString: String => J.Result[Expression] = string => {
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

  val fromJSON: JValue => J.Result[Expression] =
    J.oneOf(J.string(fromString), J.integer(fromInteger))

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
  val fromExpression: Expression => J.Result[Wire] = expr => {
    J.pass(if (Expression.isInput(expr)) {
      Wire(expr, Input)
    } else {
      Wire(Negate(expr), Output)
    })
  }

  val fromJSON: JValue => J.Result[Wire] = json =>
    J.oneOf(
      Expression.fromJSON(_).flatMap(fromExpression),
      J.jmapNamed(
        width = J.field("width", Expression.fromJSON),
        direction = J.field("direction", J.string(Direction.fromString)),
        analogDirection = J.fieldOption("analog", J.string(Direction.fromString)),
        Wire)
    )(json)
}

case class Port(name: String, wire: Wire)

object Port {
  val fromJSONField: (String, JValue) => J.Result[Port] = (name, json) => {
    Wire.fromJSON(json).map(w => Port(name, w))
  }

  val fromJSON: JValue => J.Result[Port] =
    J.jmapNamed(
      name = J.field("name", J.string),
      wire = Wire.fromJSON,
      Port)
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

  val fromJSON: (String, JValue) => J.Result[Option[IntegerConstraint]] = (name, json) => {
    (name, J.integer(json)) match {
      case ("minimum", int) => int.map(i => Some(Minimum(i)))
      case ("exclusiveMinimum", int) => int.map(i => Some(ExclusiveMinimum(i)))
      case ("maximum", int) => int.map(i => Some(Maximum(i)))
      case ("exclusiveMaximum", int) => int.map(i => Some(ExclusiveMaximum(i)))
      case _ => J.pass(None)
    }
  }

  val collectFromJSON: JValue => J.Result[Seq[IntegerConstraint]] = json => {
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

  val fromJSON: (String, JValue) => J.Result[Option[DoubleConstraint]] = (name, json) => {
    (name, J.double(json)) match {
      case ("minimum", double) => double.map(d => Some(Minimum(d)))
      case ("exclusiveMinimum", double) => double.map(d => Some(ExclusiveMinimum(d)))
      case ("maximum", double) => double.map(d => Some(Maximum(d)))
      case ("exclusiveMaximum", double) => double.map(d => Some(ExclusiveMaximum(d)))
      case _ => J.pass(None)
    }
  }

  val collectFromJSON: JValue => J.Result[Seq[DoubleConstraint]] = json => {
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

  val fromJSON: (String, JValue) => J.Result[Option[StringConstraint]] = (name, json) => {
    (name, json) match {
      case ("minLength", length) => J.integer(length).map(s => Some(MinLength(s)))
      case ("maxLength", length) => J.integer(length).map(s => Some(MaxLength(s)))
      case ("pattern", regex) => J.string(regex).map(s => Some(Pattern(s.r)))
      case _ => J.pass(None)
    }
  }

  val collectFromJSON: JValue => J.Result[Seq[StringConstraint]] = json => {
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
  
  final val fromJSON: JValue => J.Result[ParamType] = json => {
    fromJSONImp(json).flatMap {
      case value if constraints.forall(_.satisfies(value)) => J.pass(value)
      case value => J.fail(s"$value does not satisfy constraints")
    }
  }
}

object ParameterDefinition {
  val fromJSONField: (String, JValue) => J.Result[ParameterDefinition] = (name, json) => {
    J.oneOf(
      IntegerParameter.fromJSONField(name, _),
      DoubleParameter.fromJSONField(name, _),
      StringParameter.fromJSONField(name, _)
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
  val fromJSONField: (String, JValue) => J.Result[IntegerParameter] = (name, json) => {
    val namePass = (_: JValue) => J.pass(name)
    J.jmapNamed(
      default = J.fieldOption("default", J.integer),
      constraints = IntegerConstraint.collectFromJSON,
      name = namePass,
      IntegerParameter
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
  val fromJSONField: (String, JValue) => J.Result[DoubleParameter] = (name, json) => {
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
  val fromJSONField: (String, JValue) => J.Result[StringParameter] = (name, json) => {
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
  val fromJSON: JValue => J.Result[ParameterSchema] = json => {
    J.hasType("object",
      J.field("properties", J.objMap(ParameterDefinition.fromJSONField)))(json)
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
  val fromJSON: JValue => J.Result[StandardBusInterface] =
    J.jmapNamed(
      vendor = J.field("vendor", J.string),
      library = J.field("library", J.string),
      name = J.field("name", J.string),
      version = J.field("version", J.string),
      StandardBusInterface)
}


object BusInterfaceType {
  val fromJSON: JValue => J.Result[BusInterfaceType] = {
    J.oneOf(
      J.string(n => J.pass(NonStandardBusInterface(n))),
      StandardBusInterface.fromJSON
    )
  }
}

sealed trait PortMaps
case class ArrayPortMaps(ports: Seq[String]) extends PortMaps
case class FieldPortMaps(map: ListMap[String, String]) extends PortMaps

object PortMaps {
  val fromJSON: JValue => J.Result[PortMaps] = {
    J.oneOf(
      J.objMap((fieldName, portName) => J.string(portName).map(fieldName -> _))(_)
        .map(pairs => FieldPortMaps(ListMap(pairs:_*))),
      J.arrMap(J.string)(_).map(ArrayPortMaps(_))
    )
  }
}

case class BusAbstractionType(viewRef: Option[String], portMaps: Option[PortMaps])

object BusAbstractionType {
  val fromJSON: JValue => J.Result[BusAbstractionType] = {
    J.jmapNamed(
      viewRef = J.fieldOption("viewRef", J.string),
      portMaps = J.fieldOption("portMaps", PortMaps.fromJSON),
      BusAbstractionType
    )
  }
}

object BusInterface {
  val fromJSON: JValue => J.Result[BusInterface] = {
    J.map2(
      J.field("busType", BusInterfaceType.fromJSON),
      J.fieldOption("abstractionTypes", J.arrMap(BusAbstractionType.fromJSON)),
      (busType: BusInterfaceType, abstractionTypesOpt: Option[Seq[BusAbstractionType]]) =>
        BusInterface(busType, abstractionTypesOpt.getOrElse(Seq.empty))
    )
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
  val fromJSON: JValue => J.Result[BusWireDefinition] = {
    J.jmapNamed(
      presence = J.field("presence", J.string {
        case "optional" => J.pass(Optional)
        case "required" => J.pass(Required)
        case other => J.fail(s"'$other' is not a valid bus wire presence value")
      }),
      width = J.fieldOption("width", Expression.fromJSON)(_: JValue)
        .map(_.getOrElse(IntegerLiteral(BigInt(1)))),
      direction = J.field("direction", Direction.fromJSON),
      BusWireDefinition
    )
  }
}

object BusPortDefinition {
  case class WirePair(onMaster: BusWireDefinition, onSlave: BusWireDefinition)

  val fromJSON: (String, JValue) => J.Result[BusPortDefinition] = (fieldName, json) => {
    val namePass = (_: JValue) => J.pass(fieldName)
    J.jmapNamed(
      name = namePass,
      description = J.fieldOption("description", J.string),
      onMaster = J.field("wire",
        J.field("onMaster", BusWireDefinition.fromJSON)),
      onSlave = J.field("wire",
        J.field("onSlave", BusWireDefinition.fromJSON)),
      defaultValue = J.fieldOption("defaultValue", J.integer),
      isClock = J.fieldOption("isClock", J.boolean)(_: JValue).map(_.getOrElse(false)),
      requiresDriver = J.fieldOption("requiresDriver", J.boolean)(_: JValue).map(_.getOrElse(false)),
      BusPortDefinition
    )(json)
  }
}

case class BusDefinition(
  busType: StandardBusInterface,
  description: Option[String],
  ports: Seq[BusPortDefinition])

object BusDefinition {
  val fromJSON: JValue => J.Result[BusDefinition] = {
    J.jmapNamed(
      ports = J.field("ports", J.objMap(BusPortDefinition.fromJSON)),
      description = J.fieldOption("description", J.string),
      busType = StandardBusInterface.fromJSON(_),
      BusDefinition
    )
  }
}
