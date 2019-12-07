// See LICENSE for license details.
package duh.scala

import java.io.File
import duh.scala.{decoders => J}
import chisel3._
import org.json4s.jackson.JsonMethods.parse
import duh.scala.{types => DUH}

object DUHScala extends App {
  args match {
    case Array(paramsJSON, duhDoc) =>
      val parsed = parse(new File(duhDoc))
      val params = parse(new File(paramsJSON))
      val component = J.field("component", duh.scala.types.Component.fromJSON)(parsed)
      println(component.map(comp => Driver.toFirrtl(Driver.elaborate(() => new Module {
        val bbox = J.get(duh.scala.exporters.blackBox(params, comp))
        val io = IO(bbox.io.cloneType)
        io <> bbox.io
      })).serialize))
      println(component)
    case _ =>
      Console.err.println("Must provide exactly two arguments for params file path and DUH document file path")
      System.exit(1)
  }
}
