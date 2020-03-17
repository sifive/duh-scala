// See LICENSE for license details.
package duh.scala

import java.io.File
import duh.{decoders => J}
import chisel3._
import duh.scala.{types => DUH}
import duh.scala.implementation.AXI4BusImplementation
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy.{LazyModule, SimpleLazyModule, LazyModuleImp}
import freechips.rocketchip.diplomacy.{SimpleLazyModule, ValName, NexusNode, NodeImp}
import chipsalliance.rocketchip.config.Parameters

import upickle.default

object TesterNode {
  def apply[SourceParams, SinkParams, EdgeOut, EdgeIn, Bundle <: Data](
    imp: NodeImp[SourceParams, SinkParams, EdgeOut, EdgeIn, Bundle],
    defaultSourceParams: Option[SourceParams] = None,
    defaultSinkParams: Option[SinkParams] = None): NexusNode[SourceParams, SinkParams, EdgeOut, EdgeIn, Bundle] =
  new NexusNode(imp)(
    dFn = s => defaultSourceParams.getOrElse(s.headOption.getOrElse(
      throw new Exception("no source nodes connected"))),
    uFn = s => defaultSinkParams.getOrElse(s.headOption.getOrElse(
      throw new Exception("no sink nodes connected"))),
    inputRequiresOutput = false,
    outputRequiresInput = false)(
      ValName(imp.getClass.getSimpleName + "TesterNode"))

  def axi4(): NexusNode[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle] =
    apply(AXI4Imp,
      Some(AXI4MasterPortParameters(Seq(AXI4MasterParameters("TestAXI4MasterParams")))))
}

object DUHScala extends App {
  val parse = duh.json.Parser.parse(_)

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
          wrapper.nodes.ctrl.asInstanceOf[AXI4SlaveNode] := TesterNode.axi4()
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
