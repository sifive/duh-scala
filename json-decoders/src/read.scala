package duh.json

import ujson.IndexedValue
import upickle.core.{Visitor, SimpleVisitor, ObjVisitor, ArrVisitor}

import scala.collection.mutable

sealed trait JValue {
  def index: Int
}
case class JString(index: Int, value: String) extends JValue
case class JInteger(index: Int, value: BigInt) extends JValue
case class JDecimal(index: Int, value: BigDecimal) extends JValue
case class JBoolean(index: Int, value: Boolean) extends JValue
case class JNull(index: Int) extends JValue
case class JObject(index: Int, value: Map[String, JValue]) extends JValue
case class JArray(index: Int, value: Seq[JValue]) extends JValue

object Parser {
  private object Visitor extends SimpleVisitor[JValue, JValue] {
    def expectedMsg: String = "expected duh.JValue"
    override def visitNull(index: Int) = JNull(index)
    override def visitTrue(index: Int) =  JBoolean(index, true)
    override def visitFalse(index: Int) = JBoolean(index, false)

    override def visitString(s: CharSequence, index: Int) = JString(index, s.toString)
    override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) =
      if (decIndex == -1) {
        JInteger(index, BigInt(s.toString))
      } else {
        JDecimal(index, BigDecimal(s.toString))
      }
    override def visitObject(length: Int, index: Int) =
      new ObjVisitor[JValue, JObject] {
        val out = mutable.Buffer.empty[(String, JValue)]
        var currentKey: String = _
        def subVisitor = Visitor
        def visitKey(index: Int) = Visitor
        def visitKeyValue(s: Any): Unit = currentKey = s.asInstanceOf[JString].value
        def visitValue(v: JValue, index: Int): Unit = {
          out.append((currentKey, v))
        }
        def visitEnd(index: Int): JObject = JObject(index, Map(out.toSeq:_*))
      }
    override def visitArray(length: Int, index: Int) =
      new ArrVisitor[JValue, JArray] {
        val out = mutable.Buffer.empty[JValue]
        def subVisitor = Visitor
        def visitValue(v: JValue, index: Int): Unit = {
          out.append(v)
        }
        def visitEnd(index: Int): JArray = JArray(index, Seq(out.toSeq:_*))
      }
  }

  def parse(s: ujson.Readable): JValue = s.transform(Visitor)
}
