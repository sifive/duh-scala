// See LICENSE for license details.
package duh.scala

import duh.scala.implementation.BusImplementation
import duh.scala.views.{NodeBag, SelectBag, LazyPortViewBag, LazyBusInterfaceView}
import duh.scala.types._
import org.json4s.JsonAST._
import duh.scala.views.ViewException
import duh.scala.views.ParameterBag
import freechips.rocketchip.diplomacy.BaseNode

sealed trait LazyDUHWrapper {
  def nodes: SelectBag[BaseNode]
}

final case class BusImplementationKey(
  mode: InterfaceMode,
  busType: StandardBusInterface)

object LazyDUHWrapper {
  def apply(imps: Seq[BusImplementation])(params: JValue, comp: Component): LazyDUHWrapper = {
    val impMap = imps.map(imp => BusImplementationKey(imp.mode, imp.busDefinition.busType) -> imp).toMap
    val bbParams = ParameterBag(params)
    val lazyBag = LazyPortViewBag(bbParams, comp)
    val nodeMap = (comp.busInterfaces.map { interface =>
      interface.busType match {
        case busType: StandardBusInterface =>
          val key = BusImplementationKey(interface.mode, busType)
          val imp = impMap.get(key).getOrElse(throw new ViewException(s"no implementation for ${key}"))
          val extParams = JNull
          interface.name -> imp.place(extParams, LazyBusInterfaceView(bbParams, lazyBag, interface))
        case _: NonStandardBusInterface => throw new ViewException("non-standard bus interface")
      }
    }).toMap
    new LazyDUHWrapper {
      val nodes = NodeBag(nodeMap)
    }
  }
}
