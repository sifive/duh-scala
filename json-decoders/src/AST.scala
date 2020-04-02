package duh.json5

sealed trait JValue {
  def index: Int
}
case class ParserContext()
case class JString(index: Int, value: String) extends JValue
case class JInteger(index: Int, value: BigInt) extends JValue
case class JDouble(index: Int, value: Double) extends JValue
case class JBoolean(index: Int, value: Boolean) extends JValue
case class JNull(index: Int) extends JValue
case class JObject(index: Int, value: Map[String, JValue]) extends JValue
case class JArray(index: Int, value: Seq[JValue]) extends JValue
