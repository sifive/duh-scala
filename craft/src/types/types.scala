package duh.scala.types

import duh.{decoders => J}
import org.json4s.JsonAST._
import scala.collection.immutable.ListMap
import scala.util.matching.Regex

sealed trait Component {
  def name: String
  def ports: Seq[Port]
  def busInterfaces: Seq[BusInterface]
}

object Component {
  private case class ComponentImp(
    name: String,
    ports: Seq[Port],
    busInterfaces: Seq[BusInterface]) extends Component

  val fromJSON: J.Decoder[Component] =
    J.jMapNamed[ComponentImp](
      name = J.field("name", J.string),
      ports = J.field("model",
        J.field("ports", J.oneOf(J.objMap(Port.fromJSONField), J.arrMap(Port.fromJSON)))),
      busInterfaces = J.field("busInterfaces", J.arrMap(BusInterface.fromJSON))
    )
}

sealed trait Direction
case object Input extends Direction
case object Output extends Direction
case object Inout extends Direction

object Direction {
  val fromString: String => J.Result[Direction] = {
    case "in" => J.pass(Input)
    case "out" => J.pass(Output)
    case "inout" => J.pass(Inout)
    case other => J.fail(s"$other is not a valid direction")
  }

  val fromJSON: J.Decoder[Direction] = J.string(fromString)
}

sealed trait Wire {
  def width: Expression
  def direction: Option[Direction]
  def analogDirection: Option[Direction]
}

object Wire {
  private case class WireImp(
    width: Expression,
    direction: Option[Direction],
    analogDirection: Option[Direction]) extends Wire

  val fromExpression: Expression => J.Result[Wire] = expr => {
    J.pass(WireImp(expr, None, None))
  }

  val fromJSON: J.Decoder[Wire] = json =>
    J.oneOf(
      Expression.fromJSON(_).flatMap(fromExpression),
      J.jMapNamed[WireImp](
        width = J.field("width", Expression.fromJSON),
        direction = J.field("direction", J.string(Direction.fromString))(_).map(Some(_)),
        analogDirection = J.fieldOption("analog", J.string(Direction.fromString))
      ))(json)
}

sealed trait Port {
  def name: String
  def wire: Wire
}

object Port {
  private case class PortImp(name: String, wire: Wire) extends Port

  val fromJSONField: (String, JValue) => J.Result[Port] = (name, json) => {
    Wire.fromJSON(json).map(w => PortImp(name, w))
  }

  val fromJSON: J.Decoder[Port] =
    J.jMapNamed[PortImp](
      name = J.field("name", J.string),
      wire = Wire.fromJSON,
    )
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

  val collectFromJSON: J.Decoder[Seq[IntegerConstraint]] = json => {
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

  val collectFromJSON: J.Decoder[Seq[DoubleConstraint]] = json => {
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

  val collectFromJSON: J.Decoder[Seq[StringConstraint]] = json => {
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
  
  final val fromJSON: J.Decoder[ParamType] = json => {
    val checkConstraints: ParamType => J.Result[ParamType] = {
      case value if constraints.forall(_.satisfies(value)) => J.pass(value)
      case value => J.fail(s"$value does not satisfy constraints")
    }
    J.fieldOption(name, fromJSONImp)(json).flatMap {
      case Some(value) => checkConstraints(value)
      case None => default match {
        case Some(value) => checkConstraints(value)
        case None => J.fail(s"no parameter named '$name' defined in schema")
      }
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
  def fromJSONImp(json: JValue) = J.integer(json)
}

object IntegerParameter {
  val fromJSONField: (String, JValue) => J.Result[IntegerParameter] = (name, json) => {
    val namePass = (_: JValue) => J.pass(name)
    J.jMapNamed[IntegerParameter](
      default = J.fieldOption("default", J.integer),
      constraints = IntegerConstraint.collectFromJSON,
      name = namePass,
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
    val namePass = (_: JValue) => J.pass(name)
    J.jMapNamed[DoubleParameter](
      name = namePass,
      default = J.fieldOption("default", J.double),
      constraints = DoubleConstraint.collectFromJSON,
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
    val namePass = (_: JValue) => J.pass(name)
    J.jMapNamed[StringParameter](
      name = namePass,
      default = J.fieldOption("default", J.string),
      constraints = StringConstraint.collectFromJSON,
    )(json)
  }
}

sealed trait BusInterfaceType
case class StandardBusInterface(
  vendor: String,
  library: String,
  name: String,
  version: String) extends BusInterfaceType
case class NonStandardBusInterface(name: String) extends BusInterfaceType

object StandardBusInterface {
  val fromJSON: J.Decoder[StandardBusInterface] =
    J.jMapNamed[StandardBusInterface](
      vendor = J.field("vendor", J.string),
      library = J.field("library", J.string),
      name = J.field("name", J.string),
      version = J.field("version", J.string),
    )
}


object BusInterfaceType {
  val fromJSON: J.Decoder[BusInterfaceType] = {
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
  val fromJSON: J.Decoder[PortMaps] = {
    J.oneOf(
      J.objMap((fieldName, portName) => J.string(portName).map(fieldName -> _))(_)
        .map(pairs => FieldPortMaps(ListMap(pairs:_*))),
      J.arrMap(J.string)(_).map(ArrayPortMaps(_))
    )
  }
}

sealed trait BusProperty {
  def name: String
  def value: Expression
}

object BusProperty {
  private case class BusPropertyImp(
    name: String,
    value: Expression
  ) extends BusProperty

  val fromJSONField: (String, JValue) => J.Result[BusProperty] = (name, json) => {
    val namePass = (_: JValue) => J.pass(name)
    J.jMapNamed[BusPropertyImp](
      name = namePass,
      value = Expression.fromJSON,
    )(json)
  }
}

sealed trait BusRTLView {
  def portMaps: PortMaps
  def properties: JValue
}

object BusRTLView {
  private case class BusRTLViewImp(
    portMaps: PortMaps,
    properties: JValue) extends BusRTLView

  val fromJSON: J.Decoder[BusRTLView] = {
    J.jMapNamed[BusRTLViewImp](
      portMaps = J.field("portMaps", PortMaps.fromJSON),
      properties = J.fieldOption("props", J.pass)(_)
        .map(_.getOrElse(JObject(List.empty))),
    )
  }
}


sealed trait InterfaceMode
case object Source extends InterfaceMode // maps to 'master' in DUH
case object Sink extends InterfaceMode // maps to 'slave' in DUH
case object Monitor extends InterfaceMode // maps to 'monitor' in DUH


sealed trait BusInterface {
  def name: String
  def busType: BusInterfaceType
  def mode: InterfaceMode
  def rtlView: BusRTLView
}

object BusInterface {
  private case class BusInterfaceImp(
    name: String,
    busType: BusInterfaceType,
    mode: InterfaceMode,
    rtlView: BusRTLView
  ) extends BusInterface

  val fromJSON: J.Decoder[BusInterface] = {
    J.jMapNamed[BusInterfaceImp](
      busType = J.field("busType", BusInterfaceType.fromJSON),
      name = J.field("name", J.string),
      mode = J.field("interfaceMode", J.string {
        case "master" => J.pass(Source)
        case "slave" => J.pass(Sink)
        case "monitor" => J.pass(Monitor)
        case other => J.fail(s"$other is not a valid bus interface mode")
      }),
      rtlView = J.field("abstractionTypes", J.arrFind(
        J.field("viewRef", J.string)(_).map(_ == "RTLview").getOrElse(false),
        BusRTLView.fromJSON))
    )
  }
}

sealed trait BusPresence
case object Optional extends BusPresence
case object Required extends BusPresence

sealed trait BusWireDefinition {
  def presence: BusPresence
  def width: Expression
  def direction: Direction
}

object BusWireDefinition {
  private case class BusWireDefinitionImp(
    presence: BusPresence,
    width: Expression,
    direction: Direction) extends BusWireDefinition

  val fromJSON: J.Decoder[BusWireDefinition] = {
    J.jMapNamed[BusWireDefinitionImp](
      presence = J.field("presence", J.string {
        case "optional" => J.pass(Optional)
        case "required" => J.pass(Required)
        case other => J.fail(s"'$other' is not a valid bus wire presence value")
      }),
      width = J.fieldOption("width", Expression.fromJSON)(_: JValue)
        .map(_.getOrElse(Expression.IntLit(BigInt(1)))),
      direction = J.field("direction", Direction.fromJSON),
    )
  }
}

sealed trait BusPortDefinition {
  def name: String
  def description: Option[String]
  def onMaster: BusWireDefinition
  def onSlave: BusWireDefinition
  def defaultValue: Option[BigInt]
  def isClock: Boolean
  def requiresDriver: Boolean
}

object BusPortDefinition {
  private case class BusPortDefinitionImp(
    name: String,
    description: Option[String],
    onMaster: BusWireDefinition,
    onSlave: BusWireDefinition,
    defaultValue: Option[BigInt],
    isClock: Boolean,
    requiresDriver: Boolean) extends BusPortDefinition

  val fromJSON: (String, JValue) => J.Result[BusPortDefinition] = (fieldName, json) => {
    val namePass = (_: JValue) => J.pass(fieldName)
    J.jMapNamed[BusPortDefinitionImp](
      name = namePass,
      description = J.fieldOption("description", J.string),
      onMaster = J.field("wire",
        J.field("onMaster", BusWireDefinition.fromJSON)),
      onSlave = J.field("wire",
        J.field("onSlave", BusWireDefinition.fromJSON)),
      defaultValue = J.fieldOption("defaultValue", J.integer),
      isClock = J.fieldOption("isClock", J.boolean)(_).map(_.getOrElse(false)),
      requiresDriver = J.fieldOption("requiresDriver", J.boolean)(_).map(_.getOrElse(false)),
    )(json)
  }
}

sealed trait BusDefinition {
  def busType: StandardBusInterface
  def description: Option[String]
  def ports: Seq[BusPortDefinition]
}

object BusDefinition {
  private case class BusDefinitionImp(
    busType: StandardBusInterface,
    description: Option[String],
    ports: Seq[BusPortDefinition]) extends BusDefinition

  val fromJSON: J.Decoder[BusDefinition] = {
    J.jMapNamed[BusDefinitionImp](
      ports = J.field("ports", J.objMap(BusPortDefinition.fromJSON)),
      description = J.fieldOption("description", J.string),
      busType = StandardBusInterface.fromJSON(_),
    )
  }
}
