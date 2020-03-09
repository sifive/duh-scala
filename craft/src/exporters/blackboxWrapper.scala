// See LICENSE for license details.
package duh.scala

package object exporters {
  import duh.scala.{types => DUH}
  import duh.{decoders => J}
  import chisel3._
  import chisel3.experimental._
  import scala.collection.immutable.ListMap
  import org.json4s.JsonAST._

  def toChisel(ports: Seq[DUH.Port], params: JValue): J.Result[() => DynamicIO] = {
    val elements = ports.foldLeft(J.pass(Seq.empty[(String, () => Data)])) { case (acc, port) =>
      acc.flatMap { tail =>
        val portResult = toChisel(port.wire, params)
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

  def toChisel(wire: DUH.Wire, params: JValue): J.Result[() => Data] = {
    val direction = wire.analogDirection.getOrElse(wire.direction)
    DUH.Expression.evaluate(wire.width, params).flatMap {
      case DUH.Expression.IntLit(width) =>
        direction match {
          case Some(DUH.Output) => J.pass(() => Output(UInt(width.toInt.W)))
          case Some(DUH.Input) => J.pass(() => Input(UInt(width.toInt.W)))
          case Some(DUH.Inout) => J.pass(() => Analog(width.toInt.W))
          case None if width > 0 => J.pass(() => Input(UInt(width.toInt.W)))
          case None => J.pass(() => Output(UInt((-width).toInt.W)))
        }
      case _ => J.fail(s"type error: width of wire is not an Integer type")
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
    val params: J.Result[ListMap[String, Param]] = J.pass(ListMap.empty)

    params.flatMap { p =>
      toChisel(comp.ports, json).map { thunk =>
        () => new BlackBox(p) {
          val io = IO(thunk())
        }
      }
    }
  }
}
