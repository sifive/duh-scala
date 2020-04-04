package duh.json5

import scala.collection.immutable.ListMap

sealed trait JValue {
  def index: Int
}
case class ParserContext()
case class JString(index: Int, value: String) extends JValue
case class JInteger(index: Int, value: BigInt) extends JValue
case class JDouble(index: Int, value: Double) extends JValue
case class JBoolean(index: Int, value: Boolean) extends JValue
case class JNull(index: Int) extends JValue
case class JObject(index: Int, value: ListMap[String, JValue]) extends JValue
case class JArray(index: Int, value: Seq[JValue]) extends JValue

object JValue {
  def prettyPrint(value: JValue, indent: String = "  "): String = {
    val builder = new StringBuilder
    prettyPrintImp(builder, indent, 0, value)
    builder.toString
  }

  def prettyPrintImp(builder: StringBuilder, indent: String, level: Int, value: JValue): Unit = value match {
    case JString(_, value: String) =>
      builder.append("\"" + value + "\"")
    case JInteger(_, value: BigInt) =>
      builder.append(value.toString())
    case JDouble(_, value: Double) =>
      builder.append(value.toString())
    case JBoolean(_, true) =>
      builder.append("true")
    case JBoolean(_, false) =>
      builder.append("false")
    case JNull(_) =>
      builder.append("null")
    case JObject(_, fields: Map[String, JValue]) =>
      builder.append("{\n")
      fields.foreach { case (field, value) =>
        for (_ <- 0 until (level + 1)) { builder.append(indent) }
        builder.append("\"" + field + "\": ")
        prettyPrintImp(builder, indent, level + 1, value)
        builder.append(",\n")
      }
      for (_ <- 0 until level) { builder.append(indent) }
      builder.append("}")
    case JArray(_, elements: Seq[JValue]) =>
      builder.append("[\n")
      elements.foreach { case value =>
        for (_ <- 0 until (level + 1)) { builder.append(indent) }
        prettyPrintImp(builder, indent, level + 1, value)
        builder.append(",\n")
      }
      for (_ <- 0 until level) { builder.append(indent) }
      builder.append("]")
  }
}
