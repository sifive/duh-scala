// See LICENSE for license details.
package duh.scala

import java.io.File
import duh.scala.{decoders => J}
import chisel3._
import org.json4s.jackson.JsonMethods.parse
import duh.scala.{types => DUH}

object DUHScala extends App {
  args match {
    case Array(duhDoc) =>
      val parsed = parse(new File(duhDoc))
      val component = J.field("component", duh.scala.types.Component.fromJSON)(parsed)
      println(component.map(c => Driver.toFirrtl(Driver.elaborate(() => new Module {
        val bbox = Module(duh.scala.exporters.blackBox(c))
        val io = IO(bbox.io.cloneType)
        io <> bbox.io
      })).serialize))
      println(component.map(_.pSchema))
    case _ =>
      Console.err.println("Must provide exactly one argument for DUH document file path")
      System.exit(1)
  }
}
