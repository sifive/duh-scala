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
  def view: BusInterfaceView
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
      val 
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

  def placeLazyScope(inputs: LazyScopeInputs[ExternalParams]): LazyScopeOutputs[LazyScopeSideband] = {
    val properties = inputs.view.busProperties
    val ports = inputs.view.portMap
    val extParams = inputs.extParams

    val dataWidth = ports.required.RDATA.width
    val addrWidth = ports.required.ARADDR.width
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

    LazyScopeOutputs(axiNode)
  }

  def placeChiselScope(inputs: ChiselScopeInputs[ExternalParams, LazyScopeSideband]): ChiselScopeOutputs[ChiselScopeSideband] = {
    val bundle = inputs.sideband.in(0)._1

    val aw = {
      bundle.aw.valid: 1,
      bundle.aw.ready: -1,
      bundle.aw.bits.id: 'awIdWidth',
      bundle.aw.bits.addr: 'awAddrWidth',
      bundle.aw.bits.len: 8,
      bundle.aw.bits.size: 3,
      bundle.aw.bits.burst: 2,
      bundle.aw.bits.lock: 1,
      bundle.aw.bits.cache: 4,
      bundle.aw.bits.prot: 3,
      bundle.aw.bits.qos: 4,
      bundle.aw.bits.region: 0.U
    }

    val w = {
      bundle.w.valid: 1,
      bundle.w.ready: -1,
      bundle.w.bits.data: 'wDataWidth',
      bundle.w.bits.strb: 'wStrbWidth',
      bundle.w.bits.last: 1
    }

    val b = {
      bundle.b.valid: -1,
      bundle.b.ready: 1,
      bundle.b.bits.id: '-bIdWidth',
      bundle.b.bits.resp: -2
      // user: '-bUserWidth' // not in IP-XACT
    }

    val ar = {
      bundle.ar.valid: 1,
      bundle.ar.ready: -1,
      bundle.ar.bits.id: 'arIdWidth',
      bundle.ar.bits.addr: 'addrWidth',
      bundle.ar.bits.len: 8,
      bundle.ar.bits.size: 3,
      bundle.ar.bits.burst: 2,
      bundle.ar.bits.lock: 1,
      bundle.ar.bits.cache: 4,
      bundle.ar.bits.prot: 3,
      bundle.ar.bits.qos: 4,
      bundle.ar.bits.region: 0.U
        // user: 'arUserWidth' // not in IP-XACT
    }

    val r = {
      bundle.r.valid: -1,
      bundle.r.ready: 1,
      bundle.r.bits.id: '-rIdWidth',
      bundle.r.bits.data: '-dataWidth',
      bundle.r.bits.resp: -2,
      bundle.r.bits.last: -1
        // user: 'rUserWidth' // not in IP-XACT
    }
    
    ???
  }

  def demo(params: JValue, comp: Component): Unit = {
    println(
      placeLazyScope(new LazyScopeInputs[ExternalParams] {
        val view = BusInterfaceView(ParameterBag(params), comp, comp.busInterfaces.head)
        val extParams = AXI4BusImpExternalParams(0x4000, Nil, false, true)
      })
    )
  }
}
