package duh.scala.implementation

import duh.{decoders => J}
import duh.json._
import chisel3.{Data, fromIntToLiteral}
import scala.collection.immutable.ListMap
import freechips.rocketchip.diplomacy.{BundleBridgeSource, TransferSizes, AddressSet, Resource}
import freechips.rocketchip.amba.axi4.{AXI4SlaveNode, AXI4MasterNode, AXI4SlavePortParameters, AXI4SlaveParameters}
import duh.scala.views._
import duh.scala.types._
import freechips.rocketchip.diplomacy.{SimpleLazyModule, LazyModule, InModuleBody, BaseNode}
import chipsalliance.rocketchip.config.Parameters

trait BusImplementation {
  type Node <: BaseNode
  type ChiselScopeSideband
  type ExternalParams

  val mode: InterfaceMode
  val busDefinition: BusDefinition
  def externalParamsDecoder: J.Decoder[ExternalParams]

  def placeLazyScope(inputs: LazyScopeInputs[ExternalParams]): LazyScopeOutputs[Node]
  def placeChiselScope(inputs: ChiselScopeInputs[ExternalParams, Node]): Unit

  private[duh] def place(eParams: JValue, lazyView: LazyBusInterfaceView): Node
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
  def node: T
}

object LazyScopeOutputs {
  def apply[T](sidebandInput: T): LazyScopeOutputs[T] = new LazyScopeOutputs[T] {
    val node = sidebandInput
  }
}

trait ChiselScopeInputs[ExternalParams, Node] {
  def view: ChiselBusInterfaceView
  def extParams: ExternalParams
  def node: Node
}

object AXI4BusImplementation extends BusImplementation {
  type Node = AXI4SlaveNode
  type ChiselScopeSideband = Unit
  type ExternalParams = AXI4BusImpExternalParams

  def externalParamsDecoder = _ => J.pass(AXI4BusImpExternalParams(0x4000, Nil, false, true))

  val mode = Sink
  val busDefinition = { // FIXME
    val parsed = Parser.parse(new java.io.File("duh-scala/example-bus-spec.json"))
    val component = BusDefinition.fromJSON(parsed)
    component.getOrElse(throw new Exception("SLKDJFLKSDJKLFJ"))
  }

  def placeLazyScope(context: LazyScopeInputs[ExternalParams]): LazyScopeOutputs[Node] = {
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

  def placeChiselScope(context: ChiselScopeInputs[ExternalParams, Node]): Unit = {
    val nodeBundle = context.node.in(0)._1
    val bbBundle = context.view.portMap.required
    val bbBundleOpt = context.view.portMap.optional

    bbBundleOpt.ACLK.map(_.data := nodeBundle.aw.valid)
    bbBundleOpt.ACLKEN.map(_.data := nodeBundle.aw.valid)

    bbBundle.AWVALID.data := nodeBundle.aw.valid
    nodeBundle.aw.ready := bbBundle.AWREADY.data
    bbBundleOpt.AWID.map(_.data := nodeBundle.aw.bits.id)
    bbBundle.AWADDR.data := nodeBundle.aw.bits.addr
    bbBundle.AWLEN.data := nodeBundle.aw.bits.len
    bbBundle.AWSIZE.data := nodeBundle.aw.bits.size
    bbBundle.AWBURST.data := nodeBundle.aw.bits.burst
    bbBundleOpt.AWLOCK.map(_.data := nodeBundle.aw.bits.lock)
    bbBundleOpt.AWCACHE.map(_.data := nodeBundle.aw.bits.cache)
    bbBundleOpt.AWPROT.map(_.data := nodeBundle.aw.bits.prot)
    bbBundleOpt.AWQOS.map(_.data := nodeBundle.aw.bits.qos)
    bbBundleOpt.AWREGION.map(_.data := 0.U)

    // bbBundle.WID.map(_.data := nodeBundle.w.bits.id)
    bbBundle.WVALID.data := nodeBundle.w.valid
    nodeBundle.w.ready := bbBundle.WREADY.data
    bbBundle.WDATA.data := nodeBundle.w.bits.data
    bbBundle.WSTRB.data := nodeBundle.w.bits.strb
    bbBundleOpt.WLAST.map(_.data := nodeBundle.w.bits.last)

    nodeBundle.b.valid := bbBundle.BVALID.data
    bbBundle.BREADY.data := nodeBundle.b.ready
    nodeBundle.b.bits.id := bbBundleOpt.BID.map(_.data).getOrElse(0.U)
    nodeBundle.b.bits.resp := bbBundleOpt.BRESP.map(_.data).getOrElse(0.U)

    bbBundle.ARVALID.data := nodeBundle.ar.valid
    nodeBundle.ar.ready := bbBundle.ARREADY.data
    bbBundleOpt.ARID.map(_.data := nodeBundle.ar.bits.id)
    bbBundle.ARADDR.data := nodeBundle.ar.bits.addr
    bbBundle.ARLEN.data := nodeBundle.ar.bits.len
    bbBundleOpt.ARSIZE.map(_.data := nodeBundle.ar.bits.size)
    bbBundleOpt.ARBURST.map(_.data := nodeBundle.ar.bits.burst)
    bbBundleOpt.ARLOCK.map(_.data := nodeBundle.ar.bits.lock)
    bbBundleOpt.ARPROT.map(_.data := nodeBundle.ar.bits.prot)
    bbBundleOpt.ARQOS.map(_.data := nodeBundle.ar.bits.qos)
    bbBundleOpt.ARREGION.map(_.data := 0.U)

    nodeBundle.r.valid := bbBundle.RVALID.data
    bbBundle.RREADY.data := nodeBundle.r.ready
    nodeBundle.r.bits.id := bbBundleOpt.RID.map(_.data).getOrElse(0.U)
    nodeBundle.r.bits.data := bbBundle.RDATA.data
    nodeBundle.r.bits.resp := bbBundleOpt.RRESP.map(_.data).getOrElse(0.U)
    nodeBundle.r.bits.last := bbBundle.RLAST.data
  }

  private[duh] def place(eParamsJSON: JValue, lazyView: LazyBusInterfaceView) = {
    implicit val p = Parameters.empty
    val eParams = externalParamsDecoder(eParamsJSON).getOrElse(throw new ViewException("could not parse external params"))
    val axinode = placeLazyScope(new LazyScopeInputs[ExternalParams] {
      val view = lazyView
      val extParams = eParams
    }).node

    InModuleBody {
      placeChiselScope(new ChiselScopeInputs[ExternalParams, Node] {
        val view = ChiselBusInterfaceView(lazyView)
        val extParams = eParams
        def node = axinode
      })
    }
    axinode
  }
}
