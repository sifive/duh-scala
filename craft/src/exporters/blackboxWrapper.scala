// See LICENSE for license details.
package duh.scala

package object exporters {
  import duh.scala.{types => DUH}
  import duh.scala.{decoders => J}
  import chisel3._
  import chisel3.experimental._
  import scala.collection.immutable.ListMap
  import org.json4s.JsonAST._

  def toChisel(ports: Seq[DUH.Port], pSchema: DUH.ParameterSchema, params: JValue): J.Result[() => DynamicIO] = {
    val elements = ports.foldLeft(J.pass(Seq.empty[(String, () => Data)])) { case (acc, port) =>
      acc.flatMap { tail =>
        val portResult = toChisel(port.wire, pSchema, params)
        portResult.map(data => (port.name -> data) +: tail)
      }
    }

    elements.map(thunks => () => DynamicIO(thunks))
  }

  case class DynamicIO(thunks: Seq[(String, () => Data)]) extends Record {
    val elements = ListMap((thunks.map { case (name, thunk) => name -> thunk() }): _*)

    override def cloneType: this.type = {
      DynamicIO(thunks).asInstanceOf[this.type]
    }
  }

  def toChisel(wire: DUH.Wire, pSchema: DUH.ParameterSchema, params: JValue): J.Result[() => Data] = {
    val direction = wire.analogDirection.getOrElse(wire.direction)
    DUH.Expression.evaluate(wire.width, pSchema, params).map { case DUH.IntegerLiteral(width) =>
      direction match {
        case DUH.Output => () => Output(UInt(width.toInt.W))
        case DUH.Input => () => Input(UInt(width.toInt.W))
        case DUH.Inout => () => Analog(width.toInt.W)
      }
    }
  }

  def toChisel(param: DUH.ParameterDefinition, json: JValue): J.Result[Param] = {
    param match {
      case param: DUH.IntegerParameter => param.fromJSON(json).map(IntParam)
      case param: DUH.DoubleParameter => param.fromJSON(json).map(DoubleParam)
      case param: DUH.StringParameter => param.fromJSON(json).map(StringParam)
    }
  }

  def blackBox(json: JValue, comp: DUH.Component): J.Result[() => BlackBox] = {
    val params = comp.pSchema.definitions
      .foldLeft(J.pass(Seq.empty[(String, Param)]))((acc, p) =>
          acc.flatMap(tail => toChisel(p, json).map { chiselParam =>
            (p.name -> chiselParam) +: tail
          })).map(params => ListMap(params.reverse:_*))

    params.flatMap { p =>
      toChisel(comp.ports, comp.pSchema, json).map { thunk =>
        () => new BlackBox(p) {
          val io = IO(thunk())
        }
      }
    }
  }
}
