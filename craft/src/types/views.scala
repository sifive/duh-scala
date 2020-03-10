package duh.scala.views

import chisel3.{Data, Record}

import scala.collection.immutable.ListMap
import duh.scala.{types => DUH}
import org.json4s.JsonAST._
import scala.language.dynamics
import scala.language.higherKinds
import duh.{decoders => J}

sealed trait LazyPortView {
  def name: String
  def width: Int
  def direction: DUH.Direction
}

sealed trait ChiselPortView extends LazyPortView {
  def data: Data
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
  implicit val doubleParams: ParameterDecoder[Double] = apply(J.double)
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

final class SelectBag[T] private(imp: String => T) extends Dynamic {
  def selectDynamic(name: String): T = imp(name)
}

object SelectBag {
  def apply[T](imp: String => T): SelectBag[T] = new SelectBag(imp)
}

final class LazyPortViewBag private[views](
  requiredImp: String => LazyPortView,
  optionalImp: String => Option[LazyPortView]) {
  val required: SelectBag[LazyPortView] = SelectBag(requiredImp)
  val optional: SelectBag[Option[LazyPortView]] = SelectBag(optionalImp)
}

final class ChiselPortViewBag private[views](
  requiredImp: String => ChiselPortView,
  optionalImp: String => Option[ChiselPortView]) {
  val required: SelectBag[ChiselPortView] = SelectBag(requiredImp)
  val optional: SelectBag[Option[ChiselPortView]] = SelectBag(optionalImp)
}

sealed trait BusInterfaceView {
  def busProperties: ExpressionBag
  def portMap: LazyPortViewBag
}

object BusInterfaceView {
  def apply(bbParams: ParameterBag, component: DUH.Component, busInterface: DUH.BusInterface): BusInterfaceView = {
    val blackBoxPortBag = BlackBoxPortBag(bbParams, component)
    new BusInterfaceView {
      val busProperties = BusPropertyBag(bbParams, busInterface)
      val portMap = BusInterfacePortMap(blackBoxPortBag, busInterface)
    }
  }
}

sealed trait BusDefinitionView {
  def busProperties: ExpressionBag
  def portMap: ChiselPortViewBag
}

object BusDefinitionView {
  def apply(bbParams: ParameterBag, component: DUH.Component, busInterface: DUH.BusInterface): BusInterfaceView = {
    val blackBoxPortBag = BlackBoxPortBag(bbParams, component)
    new BusInterfaceView {
      val busProperties = BusPropertyBag(bbParams, busInterface)
      val portMap = BusInterfacePortMap(blackBoxPortBag, busInterface)
    }
  }
}

case class DUHIO(portViewThunks: List[() => ChiselPortView]) extends Record {
  final val portViews: List[ChiselPortView] = portViewThunks.map(_())
  final val elements = ListMap((portViews.map { case view => view.name -> view.data }): _*)


  override def cloneType: this.type = {
    DUHIO(portViewThunks).asInstanceOf[this.type]
  }
}

object BlackBoxPortBag {
  private case class LazyPortViewImp(
    name: String,
    width: Int,
    direction: DUH.Direction) extends LazyPortView

  private def toLazyPortView(params: ParameterBag, port: DUH.Port): LazyPortView = {
    val width = params.evaluate[BigInt](port.wire.width).toInt
    val direction = port.wire.analogDirection.flatMap(_ => port.wire.direction)
    (width, direction) match {
      case (w, None) if w > 0 =>
        LazyPortViewImp(name = port.name, width = w, direction = DUH.Input)
      case (w, None) =>
        LazyPortViewImp(name = port.name, width = -w, direction = DUH.Output)
      case (w, Some(d)) =>
        LazyPortViewImp(name = port.name, width = w, direction = d)
    }
  }

  def apply(blackBoxParams: ParameterBag, comp: DUH.Component): LazyPortViewBag = {
    val optionalImp: String => Option[LazyPortView] =
      comp.ports.map(port => port.name -> toLazyPortView(blackBoxParams, port)).toMap.get(_)

    val requiredImp: String => LazyPortView = name =>
      optionalImp(name).getOrElse(throw new ViewException(s"no port named $name"))

    new LazyPortViewBag(requiredImp, optionalImp)
  }

  def apply(io: DUHIO): ChiselPortViewBag = {
    val getByName: String => Option[ChiselPortView] =
      io.portViews.map(view => view.name -> view).toMap.get(_)

    new ChiselPortViewBag(
      requiredImp = name => getByName(name).getOrElse(throw new ViewException(s"no port named $name")),
      optionalImp = getByName)
  }
}

object BusInterfacePortMap {
  def apply(blackBoxPortBag: LazyPortViewBag, busInterface: DUH.BusInterface): LazyPortViewBag = {

    val getByNameOpt = busInterface.rtlView.portMaps match {
      case DUH.FieldPortMaps(map) => Some(map.get(_: String))
      case _ => None
    }

    val getByIndexOpt = (busInterface.rtlView.portMaps match {
      case DUH.ArrayPortMaps(ports) => Some((ports.zipWithIndex.map { case (e, i) => i -> e }).toMap.get(_: Int))
      case _ => None
    })

    def forceName(name: String): String =
      getByNameOpt.getOrElse(throw new ViewException(s"bus interface has a array port map"))(name)
        .getOrElse(throw new ViewException(s"no port map with key $name"))

    def forceIndex(index: Int): String =
        getByIndexOpt.getOrElse(throw new ViewException(s"bus interface has an object port map"))(index)
          .getOrElse(throw new ViewException(s"index $index is out of bounds"))
/*
    val optionalBag = new OptionalPortBag {
      def selectDynamic(name: String) = blackBoxPortBag.optional.selectDynamic(forceName(name))
      def applyDynamic(index: Int) = blackBoxPortBag.optional.selectDynamic(forceIndex(index))
    }

    val requiredBag = new RequiredPortBag {
      def selectDynamic(name: String) = blackBoxPortBag.required.selectDynamic(forceName(name))
      def applyDynamic(index: Int) = blackBoxPortBag.required.selectDynamic(forceIndex(index))
    }
*/
    new LazyPortViewBag(
      requiredImp = name => blackBoxPortBag.required.selectDynamic(forceName(name)),
      optionalImp = name => blackBoxPortBag.optional.selectDynamic(forceName(name))
    )
  }
}
