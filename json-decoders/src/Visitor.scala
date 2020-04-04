package duh.json5

import scala.collection.mutable
import scala.collection.immutable.ListMap

sealed trait BaseVisitor[Context, T] {
  def getContext(lexer: Lexer): Context
}

trait Visitor[Context, T] extends BaseVisitor[Context, T] {
  def visitNull(ctx: Context): T
  def visitTrue(ctx: Context): T
  def visitFalse(ctx: Context): T
  def visitNaN(ctx: Context, isNegative: Boolean): T
  def visitInfinity(ctx: Context, isNegative: Boolean): T

  def visitString(ctx: Context, string: String): T
  def visitInteger(ctx: Context, base: Int, number: String): T
  def visitFloat(ctx: Context, dotIndex: Int, number: String): T

  def objectVisitor(ctx: Context): ObjectVisitor[Context, T]
  def arrayVisitor(ctx: Context): ArrayVisitor[Context, T]
}


sealed trait SubVisitor[Context, T] extends BaseVisitor[Context, T]

trait ArrayVisitor[Context, T] extends SubVisitor[Context, T] {
  def elementVisitor(): Visitor[Context, T]

  def visitElement(elem: T): Unit
  def end(ctx: Context): T
}

trait ObjectVisitor[Context, T] extends SubVisitor[Context, T] {
  def valueVisitor(): Visitor[Context, T]

  def visitKey(ctx: Context, key: String): Unit
  def visitValue(value: T): Unit
  def end(ctx: Context): T
}

object Visitor {
  type Context = Int
  def apply(): Visitor[Context, JValue] = new Visitor[Context, JValue] {
    def getContext(lexer: Lexer) = lexer.getStartOffset

    def visitNull(ctx: Context): JNull = JNull(ctx)
    def visitTrue(ctx: Context): JBoolean = JBoolean(ctx, true)
    def visitFalse(ctx: Context): JBoolean = JBoolean(ctx, false)
    def visitNaN(ctx: Context, isNegative: Boolean): JDouble = {
      if (isNegative) {
        JDouble(ctx, -Double.NaN)
      } else {
        JDouble(ctx, Double.NaN)
      }
    }

    def visitInfinity(ctx: Context, isNegative: Boolean): JDouble = {
      if (isNegative) {
        JDouble(ctx, Double.NegativeInfinity)
      } else {
        JDouble(ctx, Double.PositiveInfinity)
      }
    }

    def visitString(ctx: Context, string: String): JString = JString(ctx, string)
    def visitInteger(ctx: Context, base: Int, number: String): JInteger =
      JInteger(ctx, BigInt(number, base))
    def visitFloat(ctx: Context, dotIndex: Int, number: String): JDouble =
      JDouble(ctx, number.toDouble)

    def objectVisitor(ctx: Context) =
      new ObjectVisitor[Context, JValue] {
        private val startContext: Context = ctx
        private val buffer = mutable.Buffer.empty[(String, JValue)]
        private var currentKey: String = null
        def getContext(lexer: Lexer) = lexer.getStartOffset

        def valueVisitor() = Visitor()

        def visitKey(ctx: Context, key: String): Unit = {
          currentKey = key
        }
        def visitValue(value: JValue): Unit = {
          buffer.append(currentKey -> value)
        }
        def end(ctx: Context): JValue = {
          JObject(startContext, ListMap(buffer: _*))
        }
      }

    def arrayVisitor(ctx: Context) =
      new ArrayVisitor[Context, JValue] {
        private val startContext: Context = ctx
        private val buffer = mutable.Buffer.empty[JValue]
        def getContext(lexer: Lexer) = lexer.getStartOffset

        def elementVisitor() = Visitor()
        def visitElement(element: JValue) = {
          buffer.append(element)
        }
        def end(ctx: Context): JArray = {
          JArray(startContext, Seq(buffer: _*))
        }
      }
  }
}
