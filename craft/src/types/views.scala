package duh.scala.views

import chisel3.{Module, BlackBox, Data, Record, fromIntToWidth, UInt}

import scala.collection.immutable.ListMap
import duh.scala.{types => DUH}
import duh.json._
import scala.language.dynamics
import scala.language.higherKinds
import duh.{decoders => J}

import scala.collection.immutable.ListMap
import freechips.rocketchip.diplomacy.{BaseNode, InModuleBody}

sealed trait LazyPortView {
  def name: String
  def width: Int
  def direction: DUH.Direction
  private[views] def data: Data
  private[views] def paramBag: ParameterBag
}

object LazyPortView {
  private[views] def toChiselType(view: LazyPortView): Data = {
    view.direction match {
      case DUH.Output => chisel3.Output(UInt(view.width.W))
      case DUH.Input => chisel3.Input(UInt(view.width.W))
      case DUH.Inout => chisel3.experimental.Analog(view.width.W)
    }
  }

  private[views] def apply(params: ParameterBag, port: DUH.Port, datax: => Data): LazyPortView = {
    var rawWidth = params.evaluate[BigInt](port.wire.width)
    var widthx = rawWidth match {
      case width if width < Int.MaxValue => width.toInt
      case tooBig => throw new ViewException(s"width of port ${port.name} is greater than Int.MaxValue")
    }
    val directionOpt = port.wire.analogDirection.flatMap(_ => port.wire.direction)
    var directionx: DUH.Direction = null
    (widthx, directionOpt) match {
      case (w, None) if w > 0 =>
        directionx = DUH.Input
      case (w, None) =>
        widthx = -w
        directionx = DUH.Output
      case (w, Some(d)) =>
        directionx = d
    }
    new LazyPortView {
      val name = port.name
      val width = widthx
      val direction = directionx
      lazy val data = datax
      def paramBag = params
    }
  }
}


sealed trait ParameterDecoder[T] {
  private[views] val decoder: J.Decoder[T]
}

object ParameterDecoder {
  def apply[T](dec: J.Decoder[T]): ParameterDecoder[T] =
    new ParameterDecoder[T] {
      val decoder = dec
    }

  implicit def intParams: ParameterDecoder[BigInt] = apply(J.integer)
  implicit def decimalParams: ParameterDecoder[BigDecimal] = apply(J.decimal)
  implicit def stringParams: ParameterDecoder[String] = apply(J.string)
  implicit def booleanParams: ParameterDecoder[Boolean] = apply(J.boolean)
  implicit def bagParams: ParameterDecoder[ParameterBag] = apply(json => J.pass(ParameterBag(json)))
  implicit def exprParams: ParameterDecoder[DUH.Expression] = apply(DUH.Expression.fromJSON)
}


sealed trait ExpressionEvaluator[T] {
  def apply(params: ParameterBag, expr: DUH.Expression): T
}

object ExpressionEvaluator {
  implicit val intExpressions: ExpressionEvaluator[BigInt] = new ExpressionEvaluator[BigInt] {
    def apply(params: ParameterBag, expr: DUH.Expression): BigInt =
      DUH.Expression.evaluate(expr, params.jvalue).map {
        case DUH.Expression.IntLit(i) => i
        case _ => throw new ViewException(s"expression '${expr}' has the wrong type")
      } match {
        case Right(i) => i
        case Left(e) => throw new ViewException(e.cause)
      }
  }

  implicit val decExpressions: ExpressionEvaluator[BigDecimal] = new ExpressionEvaluator[BigDecimal] {
    def apply(params: ParameterBag, expr: DUH.Expression): BigDecimal =
      DUH.Expression.evaluate(expr, params.jvalue).map {
        case DUH.Expression.DecLit(i) => i
        case _ => throw new ViewException(s"expression '${expr}' has the wrong type")
      } match {
        case Right(i) => i
        case Left(e) => throw new ViewException(e.cause)
      }
  }

  implicit val boolExpressions: ExpressionEvaluator[Boolean] = new ExpressionEvaluator[Boolean] {
    def apply(params: ParameterBag, expr: DUH.Expression): Boolean =
      DUH.Expression.evaluate(expr, params.jvalue).map {
        case DUH.Expression.BoolLit(i) => i
        case _ => throw new ViewException(s"expression '${expr}' has the wrong type")
      } match {
        case Right(i) => i
        case Left(e) => throw new ViewException(e.cause)
      }
  }
}


sealed trait RequiredParameterBag extends Dynamic {
  def selectDynamic[T: ParameterDecoder](name: String): T
}

sealed trait OptionalParameterBag extends Dynamic {
  def selectDynamic[T: ParameterDecoder](name: String): Option[T]
  def applyDynamic[T: ParameterDecoder](name: String)(default: => T): T =
    selectDynamic(name).getOrElse(default)
}

sealed trait ParameterBag {
  private[views] def jvalue: JValue
  def required: RequiredParameterBag
  def optional: OptionalParameterBag
  def evaluate[T: ExpressionEvaluator](expr: DUH.Expression): T
}

class ViewException(msg: String) extends Exception(msg)

object ParameterBag {
  def apply(j: JValue): ParameterBag = new ParameterBag {
    val jvalue = j
    val optional = new OptionalParameterBag {
      def selectDynamic[T: ParameterDecoder](name: String): Option[T] =
        J.fieldOption(name, implicitly[ParameterDecoder[T]].decoder)(jvalue).getOrElse {
          throw new ViewException(s"optional parameter '$name' has the wrong type")
        }
    }

    val required = new RequiredParameterBag {
      def selectDynamic[T: ParameterDecoder](name: String): T =
        optional.selectDynamic[T](name).getOrElse {
          throw new ViewException(s"required parameter '$name' does not exist")
        }
    }

    def evaluate[T: ExpressionEvaluator](expr: DUH.Expression): T =
      implicitly[ExpressionEvaluator[T]].apply(this, expr)
  }
}

sealed trait RequiredExpressionBag extends Dynamic {
  def selectDynamic[T: ExpressionEvaluator](name: String): T
}

sealed trait OptionalExpressionBag extends Dynamic {
  def selectDynamic[T: ExpressionEvaluator](name: String): Option[T]
  def applyDynamic[T: ExpressionEvaluator](name: String)(default: => T): T =
    selectDynamic(name).getOrElse(default)
}

sealed trait ExpressionBag {
  def required: RequiredExpressionBag
  def optional: OptionalExpressionBag
}


trait ParameterBagView {
  private[views] def underlying: ParameterBag
}

trait BusPropertyView extends ParameterBagView {
  def busProperties(busInterface: DUH.BusInterface): ExpressionBag
}

object BusPropertyBag {
  private[views] def apply(blackBoxParams: ParameterBag, busInterface: DUH.BusInterface): ExpressionBag = {
    val json = busInterface.rtlView.properties
    new ExpressionBag {
      val optional = new OptionalExpressionBag {
        def selectDynamic[T: ExpressionEvaluator](name: String): Option[T] = {
          val exprOpt = J.fieldOption(name, DUH.Expression.fromJSON)(json).getOrElse {
            throw new ViewException(s"property '$name' is not a valid expression")
          }
          exprOpt.map(blackBoxParams.evaluate[T])
        }
      }

      val required = new RequiredExpressionBag {
        def selectDynamic[T: ExpressionEvaluator](name: String): T = {
          optional.selectDynamic[T](name).getOrElse {
            throw new ViewException(s"required property '$name' does not exist")
          }
        }
      }
    }
  }
}

final class SelectBag[T] private (imp: String => T) extends Dynamic {
  private[views] def map[B](fn: T => B): SelectBag[B] =
    new SelectBag((x: String) => fn(imp(x)))

  def selectDynamic(name: String): T = imp(name)
}

object SelectBag {
  def apply[T](imp: String => T): SelectBag[T] = new SelectBag(imp)
}

private class DynamicIO(thunk: () => Seq[(String, Data)]) extends Record {
  val elements = ListMap(thunk():_*)
  override def cloneType: this.type = {
    new DynamicIO(thunk).asInstanceOf[this.type]
  }
}

sealed trait LazyPortViewBag {
  def required: SelectBag[LazyPortView]
  def optional: SelectBag[Option[LazyPortView]]
  private[views] def blackbox: BlackBox
}

object LazyPortViewBag {
  def apply(blackBoxParams: ParameterBag, comp: DUH.Component): LazyPortViewBag = {
    new LazyPortViewBag {
      val portSeq: Seq[(String, LazyPortView)] = comp.ports.map { port =>
        port.name -> LazyPortView(blackBoxParams, port, blackbox.io.elements(port.name))
      }
      val portMap = portSeq.toMap
      val optional = SelectBag(portMap.get)

      val required = SelectBag(name =>
        portMap.get(name).getOrElse(throw new ViewException(s"no port named $name")))

      val wrappedValue = InModuleBody {
        Module(new BlackBox {
          val io = IO(new DynamicIO(() => portSeq.map { case (k, v) => k -> LazyPortView.toChiselType(v) }))
          override def desiredName = comp.name
        })
      }

      lazy val blackbox = wrappedValue.getWrappedValue
    }
  }
}

sealed trait LazyBusPortView {
  def name: String
  def defaultValue: Option[BigInt]
  def presence: DUH.BusPresence
  def width: DUH.Expression
  def direction: DUH.Direction
  def isClock: Boolean
  def requiresDriver: Boolean
}

sealed trait LazyBusPortViewBag {
  def required: SelectBag[LazyBusPortView]
  def optional: SelectBag[Option[LazyBusPortView]]
  private[views] def blackbox: BlackBox
}

sealed trait LazyBusInterfaceView {
  def busProperties: ExpressionBag
  def busPorts: LazyBusPortViewBag
  def portMap: LazyPortViewBag
}

object LazyBusInterfaceView {
  private def lazyBusPortView(
    getWire: DUH.BusPortDefinition => DUH.BusWireDefinition,
    portDef: DUH.BusPortDefinition): LazyBusPortView = new LazyBusPortView {
    val wire = getWire(portDef)
    def name = portDef.name
    def defaultValue = portDef.defaultValue
    def presence = wire.presence
    def width = wire.width
    def direction = wire.direction
    def isClock = portDef.isClock
    def requiresDriver = portDef.requiresDriver
  }

  def apply(
    bbParams: ParameterBag,
    blackBoxPortBag: LazyPortViewBag,
    busInterface: DUH.BusInterface,
    busDefinition: DUH.BusDefinition): LazyBusInterfaceView = {

    val getByNameOpt = busInterface.rtlView.portMaps match {
      case DUH.FieldPortMaps(map) => map.get(_: String)
      case _ => _: String => throw new ViewException(s"bus interface has an array port map")
    }

    val getByIndexOpt = busInterface.rtlView.portMaps match {
      case DUH.ArrayPortMaps(map) => map.lift(_: Int)
      case _ => _: Int => throw new ViewException(s"bus interface has an array port map")
    }

    val busPortDefMap = busDefinition.ports.map { busPortDef =>
      busPortDef.name -> busPortDef
    }.toMap

    val getWire = busInterface.mode match {
      case DUH.Source => (_: DUH.BusPortDefinition).onMaster
      case DUH.Sink => (_: DUH.BusPortDefinition).onSlave
      case DUH.Monitor => throw new ViewException("monitor bus InterfaceMode not supported")
    }

    def getBusPortView(busName: String): Option[LazyBusPortView] = {
      val portNameOpt = getByNameOpt(busName)
      val busPortDefOpt = busPortDefMap.get(busName)
      (portNameOpt, busPortDefOpt) match {
        case (_, Some(busPortDef)) =>
          getWire(busPortDef).presence match {
            case DUH.Required if !portNameOpt.isDefined =>
              throw new ViewException(s"required bus port $busName not defined in bus interface port map")
            case _ =>
          }
          val lazyPortViewOpt = portNameOpt.map(blackBoxPortBag.required.selectDynamic(_))
          Some(lazyBusPortView(getWire, busPortDef))
        case (None, None) => None
          // throw new ViewException(s"bus port $busName is not part of the bus definition")
        case (_, None) =>
          throw new ViewException(s"bus port $busName defined in bus inteface but not in corresponding bus definition")
      }
    }

    new LazyBusInterfaceView {
      val busProperties = BusPropertyBag(bbParams, busInterface)

      val busPorts = new LazyBusPortViewBag {
        val required = SelectBag { busName =>
          getBusPortView(busName)
            .getOrElse(throw new ViewException(s"required bus port $busName not found"))
        }
        val optional = SelectBag(getBusPortView)
        def blackbox: BlackBox = blackBoxPortBag.blackbox
      }

      val portMap = new LazyPortViewBag {
        val required = SelectBag { busName =>
          val portName = getByNameOpt(busName).getOrElse(throw new ViewException(s"no port map with key $busName"))
          blackBoxPortBag.required.selectDynamic(portName)
        }
        val optional = SelectBag { busName =>
          getByNameOpt(busName).flatMap {
            blackBoxPortBag.optional.selectDynamic(_)
          }
        }
        def blackbox: BlackBox = blackBoxPortBag.blackbox
      }
    }
  }
}

sealed trait ChiselPortView {
  def name: String
  def width: Int
  def direction: DUH.Direction
  def data: Data
}

object ChiselPortView {
  def apply(lazyPortView: LazyPortView): ChiselPortView = {
    new ChiselPortView {
      val name = lazyPortView.name
      val width = lazyPortView.width
      val direction = lazyPortView.direction
      val data = lazyPortView.data
    }
  }
}

sealed trait ChiselBusPortView {
  def name: String
  def defaultValue: Option[BigInt]
  def presence: DUH.BusPresence
  def width: DUH.Expression
  def direction: DUH.Direction
  def isClock: Boolean
  def requiresDriver: Boolean
}

object ChiselBusPortView {
  def apply(lazyView: LazyBusPortView): ChiselBusPortView = {
    new ChiselBusPortView {
      def name = lazyView.name
      def defaultValue = lazyView.defaultValue
      def presence = lazyView.presence
      def width = lazyView.width
      def direction = lazyView.direction
      def isClock = lazyView.isClock
      def requiresDriver = lazyView.isClock
    }
  }
}

sealed trait ChiselPortViewBag {
  def required: SelectBag[ChiselPortView]
  def optional: SelectBag[Option[ChiselPortView]]
}

sealed trait ChiselBusPortBag {
  def required: SelectBag[ChiselBusPortView]
  def optional: SelectBag[Option[ChiselBusPortView]]
}

object ChiselBusPortBag {
  def apply(lazyBag: LazyBusPortViewBag): ChiselBusPortBag = {
    lazyBag.blackbox // force chisel elaborate
    new ChiselBusPortBag {
      val required = lazyBag.required.map(ChiselBusPortView(_))
      val optional = lazyBag.optional.map(_.map(ChiselBusPortView(_)))
    }
  }
}

sealed trait ChiselBusInterfaceView {
  def busProperties: ExpressionBag
  def busPorts: LazyBusPortViewBag
  def portMap: ChiselPortViewBag
}

object ChiselBusInterfaceView {
  def apply(lazyView: LazyBusInterfaceView): ChiselBusInterfaceView = {
    new ChiselBusInterfaceView {
      val busProperties = lazyView.busProperties
      val busPorts = lazyView.busPorts
      val portMap = new ChiselPortViewBag {
        val required = lazyView.portMap.required.map(ChiselPortView(_))
        val optional = lazyView.portMap.optional.map(_.map(ChiselPortView(_)))
      }
    }
  }
}

sealed trait NodeBag {
  def optional: SelectBag[Option[BaseNode]]
  def required: SelectBag[BaseNode]
}

object NodeBag {
  def apply(map: Map[String, BaseNode]): SelectBag[BaseNode] = SelectBag(map)
}
