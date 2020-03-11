// See LICENSE for license details.
package duh.scala

import java.io.File
import duh.{decoders => J}
import chisel3._
import org.json4s.jackson.JsonMethods.parse
import duh.scala.{types => DUH}
import duh.scala.implementation.AXI4BusImplementation
import freechips.rocketchip.diplomacy.{LazyModule, SimpleLazyModule}
import freechips.rocketchip.diplomacy.SimpleLazyModule
import chipsalliance.rocketchip.config.Parameters

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
          LazyDUHWrapper(Seq(AXI4BusImplementation))(params, comp)
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
