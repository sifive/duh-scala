package duh.scala.views

import chisel3.{Module, BlackBox, Data, Record, fromIntToWidth}

import scala.collection.immutable.ListMap
import duh.scala.{types => DUH}
import org.json4s.JsonAST._
import scala.language.dynamics
import scala.language.higherKinds
import duh.{decoders => J}

import scala.collection.immutable.ListMap
import freechips.rocketchip.diplomacy.BaseNode

sealed trait LazyPortView {
  def name: String
  def width: Int
  def direction: DUH.Direction
  private[views] lazy val data = direction match {
    case DUH.Output => chisel3.Output(chisel3.UInt(width.W))
    case DUH.Input => chisel3.Input(chisel3.UInt(width.W))
    case DUH.Inout => chisel3.experimental.Analog(width.W)
  }
}

object LazyPortView {
  def apply(params: ParameterBag, port: DUH.Port): LazyPortView = {
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

  implicit val intParams: ParameterDecoder[BigInt] = apply(J.integer)
  implicit val decimalParams: ParameterDecoder[BigDecimal] = apply(J.decimal)
  implicit val stringParams: ParameterDecoder[String] = apply(J.string)
  implicit val booleanParams: ParameterDecoder[Boolean] = apply(J.boolean)
  implicit val bagParams: ParameterDecoder[ParameterBag] = apply(json => J.pass(ParameterBag(json)))
  implicit val exprParams: ParameterDecoder[DUH.Expression] = apply(DUH.Expression.fromJSON)
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

sealed trait LazyPortViewBag {
  def required: SelectBag[LazyPortView]
  def optional: SelectBag[Option[LazyPortView]]
}

object LazyPortViewBag {
  def apply(blackBoxParams: ParameterBag, comp: DUH.Component): LazyPortViewBag = {
    new LazyPortViewBag {
      val portSeq = comp.ports.map(port => port.name -> LazyPortView(blackBoxParams, port))
      val portMap = portSeq.toMap
      val optional = SelectBag(portMap.get)

      val required = SelectBag(name =>
        portMap.get(name).getOrElse(throw new ViewException(s"no port named $name")))

      lazy val blackbox =  Module(new BlackBox {
        val io = IO(new DynamicIO(() => portSeq.map { case (k, v) => k -> v.data }))
      })
    }
  }
}

sealed trait LazyBusInterfaceView {
  def busProperties: ExpressionBag
  def portMap: LazyPortViewBag
}

object LazyBusInterfaceView {
  private[views] def busInterfacePortMap(blackBoxPortBag: LazyPortViewBag, busInterface: DUH.BusInterface): LazyPortViewBag = {

    val getByNameOpt = busInterface.rtlView.portMaps match {
      case DUH.FieldPortMaps(map) => map.get(_: String)
      case _ => throw new ViewException(s"bus interface has an array port map")
    }

    new LazyPortViewBag {
      val required = SelectBag { busName =>
        val portName = getByNameOpt(busName).getOrElse(throw new ViewException(s"no port map with key $busName"))
        blackBoxPortBag.required.selectDynamic(portName)
      }
      val optional = SelectBag { busName =>
        getByNameOpt(busName).flatMap {
          blackBoxPortBag.optional.selectDynamic(_)
        }
      }
    }
  }

  def apply(
    bbParams: ParameterBag,
    blackBoxPortBag: LazyPortViewBag,
    busInterface: DUH.BusInterface): LazyBusInterfaceView = {
    new LazyBusInterfaceView {
      val busProperties = BusPropertyBag(bbParams, busInterface)
      val portMap = busInterfacePortMap(blackBoxPortBag, busInterface)
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

sealed trait ChiselPortBag {
  def required: SelectBag[ChiselPortView]
  def optional: SelectBag[Option[ChiselPortView]]
}

private class DynamicIO(thunk: () => Seq[(String, Data)]) extends Record {
  val elements = ListMap(thunk():_*)
  override def cloneType: this.type = {
    new DynamicIO(thunk).asInstanceOf[this.type]
  }
}

object ChiselPortBag {
  def apply(lazyBag: LazyPortViewBag): ChiselPortBag = {
    new ChiselPortBag {
      val required = lazyBag.required.map(ChiselPortView(_))
      val optional = lazyBag.optional.map(_.map(ChiselPortView(_)))
    }
  }

}

sealed trait ChiselBusInterfaceView {
  def busProperties: ExpressionBag
  def portMap: ChiselPortBag
}

object ChiselBusInterfaceView {
  def apply(lazyView: LazyBusInterfaceView): ChiselBusInterfaceView = {
    new ChiselBusInterfaceView {
      val busProperties = lazyView.busProperties
      val portMap = ChiselPortBag(lazyView.portMap)
    }
  }
}

sealed trait NodeBag {
  def optional: SelectBag[Option[BaseNode]]
  def required: SelectBag[BaseNode]
}

object NodeBag {
  def apply(map: Map[String, BaseNode]): NodeBag = new NodeBag {
    val required = SelectBag(map)
    val optional = SelectBag(map.get)
  }
}
