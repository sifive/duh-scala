package duh.scala.views

import duh.scala.{types => DUH}
import org.json4s.JsonAST._
import scala.language.dynamics
import duh.scala.{decoders => J}
import freechips.rocketchip.diplomacy.{BundleBridgeSource, TransferSizes, AddressSet, Resource}
import freechips.rocketchip.amba.axi4.{AXI4SlaveNode, AXI4MasterNode, AXI4SlavePortParameters, AXI4SlaveParameters}


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
    def apply(params: ParameterBag, expr: DUH.Expression): BigInt = expr match {
      case DUH.Parameter(name) => params.required.selectDynamic[BigInt](name)
      case DUH.Negate(child) => -apply(params, child)
      case lit: DUH.IntegerLiteral => lit.value
      case lit: DUH.Literal => throw new ViewException(s"expression '${expr}' has the wrong type")
    }
  }
}


sealed trait RequiredParameterBag extends Dynamic {
  def selectDynamic[T: ParameterDecoder](name: String): T
}

sealed trait OptionalParameterBag extends Dynamic {
  def selectDynamic[T: ParameterDecoder](name: String): Option[T]
}

sealed trait ParameterBag {
  def required: RequiredParameterBag
  def optional: OptionalParameterBag
  def evaluate[T: ExpressionEvaluator](expr: DUH.Expression): T
}

class ViewException(msg: String) extends Exception(msg)

object ParameterBag {
  def apply(j: JValue): ParameterBag = new ParameterBag {
    val json = j
    val optional = new OptionalParameterBag {
      def selectDynamic[T: ParameterDecoder](name: String): Option[T] =
        J.fieldOption(name, implicitly[ParameterDecoder[T]].decoder)(json).getOrElse {
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


sealed trait RequiredPortBag extends Dynamic {
  def selectDynamic(name: String): DUH.Port
}

sealed trait OptionalPortBag extends Dynamic {
  def selectDynamic(name: String): Option[DUH.Port]
}

sealed trait PortBag {
  def required: RequiredPortBag
  def optional: OptionalPortBag
}

sealed trait BusInterfaceView {
  def blackBoxParams: ParameterBag
  def busProperties: ExpressionBag
  def portMap: PortBag
}


object BusInterfaceView {
  def apply(bbParams: ParameterBag, component: DUH.Component, busInterface: DUH.BusInterface): BusInterfaceView = {
    val blackBoxPortBag = BlackBoxPortBag(bbParams, component)
    new BusInterfaceView {
      val blackBoxParams = bbParams
      val busProperties = BusPropertyBag(blackBoxParams, busInterface)
      val portMap = BusInterfacePortMap(blackBoxPortBag, busInterface)
    }
  }
}

object BlackBoxPortBag {
  def apply(blackBoxParams: ParameterBag, comp: DUH.Component): PortBag = {
    val getByName = comp.ports.map(port => port.name -> port).toMap.get(_)
    new PortBag {
      def optional = new OptionalPortBag {
        def selectDynamic(name: String) = getByName(name)
      }
      def required = new RequiredPortBag {
        def selectDynamic(name: String) = optional.selectDynamic(name)
          .getOrElse(throw new ViewException(s"no port named $name"))
      }
    }
  }
}

object BusInterfacePortMap {
  def apply(blackBoxPortBag: PortBag, busInterface: DUH.BusInterface): PortBag = {

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

    val optionalBag = new OptionalPortBag {
      def selectDynamic(name: String) = blackBoxPortBag.optional.selectDynamic(forceName(name))
      def applyDynamic(index: Int) = blackBoxPortBag.optional.selectDynamic(forceIndex(index))
    }

    val requiredBag = new RequiredPortBag {
      def selectDynamic(name: String) = blackBoxPortBag.required.selectDynamic(forceName(name))
      def applyDynamic(index: Int) = blackBoxPortBag.required.selectDynamic(forceIndex(index))
    }

    new PortBag {
      def optional = optionalBag
      def required = requiredBag
    }
  }
}

trait BusImplementation {
  type SlaveNode
  type MasterNode
  type ExternalParams

  def makeSlaveNode(view: BusInterfaceView, extParams: ExternalParams): SlaveNode
}

case class AXI4BusImpExternalParams(
  baseAddress: Long,
  resources: Seq[Resource],
  executable: Boolean,
  canInterleave: Boolean)

object AXI4BusImplementation extends BusImplementation {
  type SlaveNode = AXI4SlaveNode
  type MasterNode = AXI4MasterNode
  type ExternalParams = AXI4BusImpExternalParams

  def makeSlaveNode(view: BusInterfaceView, extParams: ExternalParams): SlaveNode = {
    val params = view.blackBoxParams
    val properties = view.busProperties
    val ports = view.portMap

    val dataWidth = params.evaluate[BigInt](ports.required.RDATA.wire.width).toInt
    val addrWidth = params.evaluate[BigInt](ports.required.ARADDR.wire.width).toInt
    val maxTransferSize = properties.optional.maxTransferSize[BigInt].map(_.toInt).getOrElse(dataWidth / 8)
    val writeTransferSizes = TransferSizes(1, maxTransferSize)
    val readTransferSizes = TransferSizes(1, maxTransferSize)
    val executable = extParams.executable
    val canInterleave = extParams.canInterleave // TODO: non-integer expressions: properties.optional.canInterleave[Boolean].getOrElse(true)
    val axiNode = AXI4SlaveNode(Seq(
      AXI4SlavePortParameters(
        slaves = Seq(
          AXI4SlaveParameters(
            address       = List(AddressSet(extParams.baseAddress, ((1L << addrWidth) - 1))),
            executable    = executable,
            supportsWrite = writeTransferSizes,
            supportsRead  = readTransferSizes,
            interleavedId = if (canInterleave) { Some(0) } else { None }, // TODO: need boolean params
            resources     = extParams.resources
          )
        ),
        beatBytes = dataWidth / 8
      )
    ))

    axiNode
  }

  def demo(params: JValue, comp: DUH.Component): Unit = {
    println(
      makeSlaveNode(
        BusInterfaceView(ParameterBag(params), comp, comp.busInterfaces.head),
        AXI4BusImpExternalParams(0x4000, Nil, false, true))
      )
  }
}
