// See LICENSE for license details.
package duh.scala

import java.io.File
import duh.scala.{decoders => J}
import chisel3._

case class DUHScalaArgs(
  duhDocumentFileName: String
)

object DUHScala extends App {
  def parse(args: DUHScalaArgs): ujson.Value = {
    val duhDocFile = new File(args.duhDocumentFileName)
    ujson.read(duhDocFile)
  }

  val filename = args match {
    case Array(duhDoc) =>
      val args = DUHScalaArgs(duhDoc)
      val parsed = parse(args)
      val component = J.field("component", duh.scala.types.Component.fromJSON)(parsed)
      println(component.map(c => Driver.toFirrtl(Driver.elaborate(() => new Module {
        val io = IO(new Bundle {
          val foo = Input(Bool())
        })
        val bbox = Module(duh.scala.exporters.blackBox(c))
      })).serialize))
    case _ =>
      Console.err.println("Must provide exactly one argument for DUH document file path")
      System.exit(1)
  }
}
