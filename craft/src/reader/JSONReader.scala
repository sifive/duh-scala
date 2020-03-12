// See LICENSE for license details.
package duh.scala

import java.io.File
import duh.{decoders => J}
import chisel3._
import org.json4s.jackson.JsonMethods.parse
import duh.scala.{types => DUH}
import duh.scala.implementation.AXI4BusImplementation
import freechips.rocketchip.amba.axi4.{AXI4SlaveNode, AXI4MasterParameters, AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4Imp}
import freechips.rocketchip.diplomacy.{LazyModule, SimpleLazyModule, LazyModuleImp}
import freechips.rocketchip.diplomacy.{SimpleLazyModule, ValName, NexusNode}
import chipsalliance.rocketchip.config.Parameters

sealed abstract class AXI4TesterNode(
  masterFn: Seq[AXI4MasterPortParameters] => AXI4MasterPortParameters,
  slaveFn: Seq[AXI4SlavePortParameters] => AXI4SlavePortParameters)(implicit valName: ValName) extends NexusNode(AXI4Imp)(masterFn, slaveFn, false, false)

object AXI4TesterNode {
  def testerMasterFn: Seq[AXI4MasterPortParameters] => AXI4MasterPortParameters =
    _ => AXI4MasterPortParameters(
      masters = Seq(AXI4MasterParameters("tester")),
      opaqueBits = 0,
      userBits = 0)
  def testerSlaveFn: Seq[AXI4SlavePortParameters] => AXI4SlavePortParameters = _.head
  def apply(
    masterFn: Seq[AXI4MasterPortParameters] => AXI4MasterPortParameters = testerMasterFn,
    slaveFn: Seq[AXI4SlavePortParameters] => AXI4SlavePortParameters = testerSlaveFn)
  (implicit valName: ValName = ValName("AXI4TesterNode")) =
    new AXI4TesterNode(masterFn, slaveFn) {
    }
}

object DUHScala extends App {
  if (args.length < 1) {
    Console.err.println("need at least one argument [bus|component]")
    System.exit(1)
  } else {
    val otherArgs = args.drop(1)
    args(0) match {
      case "component" => duhComponent(otherArgs)
      case "bus" => duhBus(otherArgs)
      case _ =>
        Console.err.println("first argument must be [bus|component]")
        System.exit(1)
    }
  }

  def duhComponent(args: Array[String]): Unit = args match {
    case Array(paramsJSON, duhDoc) =>
      val parsed = parse(new File(duhDoc))
      val params = parse(new File(paramsJSON))
      val component = J.field("component", duh.scala.types.Component.fromJSON)(parsed)
      println(component.map(comp => Driver.toFirrtl(Driver.elaborate(() => {
        implicit val p = Parameters.empty
        val lazyModule = LazyModule(new SimpleLazyModule {
          val wrapper = LazyDUHWrapper(Seq(AXI4BusImplementation))(params, comp)
          wrapper.nodes.ctrl.asInstanceOf[AXI4SlaveNode] := AXI4TesterNode()
        })
        lazyModule.module
      })).serialize))
    case _ =>
      Console.err.println("Must provide exactly two arguments for params file path and DUH document file path")
      System.exit(1)
  }

  def duhBus(args: Array[String]): Unit = args match {
    case Array(duhDoc) =>
      val parsed = parse(new File(duhDoc))
      val component = duh.scala.types.BusDefinition.fromJSON(parsed)
      println(J.get(component))
    case _ =>
      Console.err.println("Must provide exactly one argument for DUH bus document file path")
      System.exit(1)
  }
}
