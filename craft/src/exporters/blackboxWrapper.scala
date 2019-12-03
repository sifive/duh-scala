// See LICENSE for license details.
package duh.scala

package object exporters {
  import duh.scala.{types => DUH}
  import chisel3._
  import chisel3.core.Analog
  import scala.collection.immutable.ListMap

  case class DynamicIO(ports: Seq[DUH.Port]) extends Record {
    val elements = ListMap(ports.map(p => p.name -> toChisel(p.wire)):_*)

    override def cloneType: this.type = {
      DynamicIO(ports).asInstanceOf[this.type]
    }
  }

  def toChisel(wire: DUH.Wire): Data = {
    val direction = wire.analogDirection.getOrElse(wire.direction)
    val width = wire.width match {
      case DUH.IntegerLiteral(lit) => lit.toInt.W
      case _ => 1234.W
    }

    direction match {
      case DUH.Output => Output(UInt(width))
      case DUH.Input => Input(UInt(width))
      case DUH.Inout => Analog(width)
    }
  }

  def blackBox(comp: DUH.Component): BlackBox = {
    new BlackBox {
      val io = IO(DynamicIO(comp.ports))
    }
  }
}
