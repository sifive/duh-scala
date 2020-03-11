package duh.scala.implementation

import chisel3.{Data, fromIntToLiteral}
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
  def view: LazyBusInterfaceView
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
  def view: ChiselBusInterfaceView
  def extParams: ExternalParams
  def sideband: Sideband
}

sealed trait ChiselScopeOutputs[T] {
  def sideband: T
}

object ChiselScopeOutputs {
  def apply[T](sidebandInput: T): ChiselScopeOutputs[T] =
    new ChiselScopeOutputs[T] {
      val sideband = sidebandInput
    }
}

object AXI4BusImplementation extends BusImplementation {
  type LazyScopeSideband = AXI4SlaveNode
  type ChiselScopeSideband = Unit
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
    val nodeBundle = context.sideband.in(0)._1
    val bbBundle = context.view.portMap.required
    val bbBundleOpt = context.view.portMap.optional

    bbBundleOpt.ACLK.map(_.data := nodeBundle.aw.valid)
    bbBundleOpt.ACLKEN.map(_.data := nodeBundle.aw.valid)

    bbBundle.AWVALID.data := nodeBundle.aw.valid
    nodeBundle.aw.ready := bbBundle.AWREADY.data
    bbBundle.AWID.data := nodeBundle.aw.bits.id
    bbBundle.AWADDR.data := nodeBundle.aw.bits.addr
    bbBundle.AWLEN.data := nodeBundle.aw.bits.len
    bbBundle.AWSIZE.data := nodeBundle.aw.bits.size
    bbBundle.AWBURST.data := nodeBundle.aw.bits.burst
    bbBundleOpt.AWLOCK.map(_.data := nodeBundle.aw.bits.lock)
    bbBundleOpt.AWCACHE.map(_.data := nodeBundle.aw.bits.cache)
    bbBundleOpt.AWPROT.map(_.data := nodeBundle.aw.bits.prot)
    bbBundleOpt.AWQOS.map(_.data := nodeBundle.aw.bits.qos)
    bbBundleOpt.AWREGION.map(_.data := 4.U)

    //bbBundle.WID.map(_.data := nodeBundle.w.bits.id)
    bbBundle.WVALID.data := nodeBundle.w.valid
    nodeBundle.w.ready := bbBundle.WREADY.data
    bbBundle.WDATA.data := nodeBundle.w.bits.data
    bbBundle.WSTRB.data := nodeBundle.w.bits.strb
    bbBundleOpt.WLAST.map(_.data := nodeBundle.w.bits.last)

    bbBundle.BVALID.data := nodeBundle.b.valid
    nodeBundle.b.ready := bbBundle.BREADY.data
    bbBundleOpt.BID.map(_.data := nodeBundle.b.bits.id)
    bbBundleOpt.BRESP.map(_.data := nodeBundle.b.bits.resp)

    bbBundle.ARVALID.data := nodeBundle.ar.valid
    nodeBundle.ar.ready := bbBundle.ARREADY.data
    bbBundleOpt.ARID.map(_.data := nodeBundle.ar.bits.id)
    bbBundle.ARADDR.data := nodeBundle.ar.bits.addr
    bbBundle.ARLEN.data := nodeBundle.ar.bits.len
    bbBundle.ARSIZE.data := nodeBundle.ar.bits.size
    bbBundle.ARBURST.data := nodeBundle.ar.bits.burst
    bbBundleOpt.ARLOCK.map(_.data := nodeBundle.ar.bits.lock)
    bbBundleOpt.ARPROT.map(_.data := nodeBundle.ar.bits.prot)
    bbBundleOpt.ARQOS.map(_.data := nodeBundle.ar.bits.qos)
    bbBundleOpt.ARREGION.map(_.data := 4.U)

    bbBundle.RVALID.data := nodeBundle.r.valid
    nodeBundle.r.ready := bbBundle.RREADY.data
    bbBundleOpt.RID.map(_.data := nodeBundle.r.bits.id)
    bbBundle.RDATA.data := nodeBundle.r.bits.data
    bbBundleOpt.RRESP.map(_.data := nodeBundle.r.bits.resp)
    bbBundle.RLAST.data := nodeBundle.r.bits.last

    ChiselScopeOutputs(Unit)
  }

  def demo(params: JValue, comp: Component): Unit = {
    println(
      placeLazyScope(new LazyScopeInputs[ExternalParams] {
        val view = LazyBusInterfaceView(ParameterBag(params), comp, comp.busInterfaces.head)
        val extParams = AXI4BusImpExternalParams(0x4000, Nil, false, true)
      }).sideband
    )
  }
}
