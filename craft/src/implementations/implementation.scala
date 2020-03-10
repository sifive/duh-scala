package duh.scala.implementation

import chisel3.{Data}
import scala.collection.immutable.ListMap
import freechips.rocketchip.diplomacy.{BundleBridgeSource, TransferSizes, AddressSet, Resource}
import freechips.rocketchip.amba.axi4.{AXI4SlaveNode, AXI4MasterNode, AXI4SlavePortParameters, AXI4SlaveParameters}
import duh.scala.views._
import org.json4s.JsonAST._
import duh.scala.types.{InterfaceMode, Sink, Component, BusDefinition, BusPortDefinition}

trait BusImplementation {
  type LazyScopeSideband
  type ChiselScopeSideband
  type ExternalParams

  def mode: InterfaceMode
  def busDefinition: BusDefinition

  def placeLazyScope(inputs: LazyScopeInputs[ExternalParams]): LazyScopeOutputs[LazyScopeSideband]
  def placeChiselScope(inputs: ChiselScopeInputs[ExternalParams, LazyScopeSideband]): ChiselScopeOutputs[ChiselScopeSideband]
}

case class AXI4BusImpExternalParams(
  baseAddress: Long,
  resources: Seq[Resource],
  executable: Boolean,
  canInterleave: Boolean)

sealed trait LazyScopeInputs[T] {
  def view: BusInterfaceView
  def extParams: T
}

sealed trait LazyScopeOutputs[T] {
  def sideband: T
}

object LazyScopeOutputs {
  def apply[T](sidebandInput: T): LazyScopeOutputs[T] = new LazyScopeOutputs[T] {
    val sideband = sidebandInput
  }
}

trait ChiselScopeInputs[ExternalParams, Sideband] {
  def view: BusDefinitionView
  def extParams: ExternalParams
  def sideband: Sideband
}

sealed trait ChiselScopeOutputs[T] {
  def mapping: ListMap[BusPortDefinition, Data]
  def sideband: T
}

object ChiselScopeOutputs {
  def apply[T](mappingInput: ListMap[BusPortDefinition, Data], sidebandInput: T): ChiselScopeOutputs[T] =
    new ChiselScopeOutputs[T] {
      val mapping = mappingInput
      val sideband = sidebandInput
      ???
    }
}

object Connect {
  def applySlave(portBag: ChiselPortViewBag, outputs: ChiselScopeOutputs[_]): Unit = {
    val mapping = outputs.mapping
    mapping.map { case (portDef, data) =>
      portDef.onSlave
    }
  }
}

object AXI4BusImplementation extends BusImplementation {
  type LazyScopeSideband = AXI4SlaveNode
  type ChiselScopeResult = Unit
  type ExternalParams = AXI4BusImpExternalParams

  val mode = Sink
  val busDefinition = { // FIXME
    val parsed = org.json4s.jackson.JsonMethods.parse(new java.io.File("duh-scala/example-bus-spec.json"))
    val component = BusDefinition.fromJSON(parsed)
    component.getOrElse(throw new Exception("SLKDJFLKSDJKLFJ"))
  }

  def placeLazyScope(context: LazyScopeInputs[ExternalParams]): LazyScopeOutputs[LazyScopeSideband] = {
    val properties = context.view.busProperties
    val ports = context.view.portMap
    val extParams = context.extParams

    val dataWidth = ports.required.RDATA.width
    val addrWidth = ports.required.ARADDR.width
    val maxTransferSize =
      properties
        .optional
        .maxTransferSize[BigInt]
        .map(_.toInt)
        .getOrElse(dataWidth / 8)
    val canInterleave =
      properties
        .optional
        .canInterleave[Boolean](false)
    val axiNode = AXI4SlaveNode(Seq(
      AXI4SlavePortParameters(
        slaves = Seq(
          AXI4SlaveParameters(
            address = List(AddressSet(extParams.baseAddress, ((1L << addrWidth) - 1))),
            executable = extParams.executable,
            supportsWrite = TransferSizes(1, maxTransferSize),
            supportsRead = TransferSizes(1, maxTransferSize),
            interleavedId = if (canInterleave) { Some(0) } else { None },
            resources = extParams.resources
          )
        ),
        beatBytes = dataWidth / 8
      )
    ))

    LazyScopeOutputs(axiNode)
  }

  def placeChiselScope(context: ChiselScopeInputs[ExternalParams, LazyScopeSideband]): ChiselScopeOutputs[ChiselScopeSideband] = {
    ???
  }

  def demo(params: JValue, comp: Component): Unit = {
    println(
      placeLazyScope(new LazyScopeInputs[ExternalParams] {
        val view = BusInterfaceView(ParameterBag(params), comp, comp.busInterfaces.head)
        val extParams = AXI4BusImpExternalParams(0x4000, Nil, false, true)
      }).sideband
    )
  }
}
