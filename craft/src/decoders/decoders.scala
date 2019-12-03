// See LICENSE for license details.
package duh.scala

import org.json4s.JsonAST._

package object decoders {
  sealed trait Error {
    def path: String
    override def toString: String = Error.toString(this)
  }

  case object Error {
    case class Field(
      field: String,
      error: Error
    ) extends Error {
      def path: String = s".${field}" + error.path
    }

    case class Index(
      index: Int,
      error: Error
    ) extends Error {
      def path: String = s"[$index]" + error.path
    }

    case class Multiple(errors: Seq[Error]) extends Error {
      def path: String = ""
    }

    case class Failure(
      cause: String
    ) extends Error {
      val path: String = ""
    }

    def toString(error: Error): String = {
      val tab = "  "
      def helper(indent: Int, error: Error): String = error match {
        case Failure(cause) => s": $cause"
        case Field(field, child) => s".$field${helper(indent, child)}"
        case Index(idx, child) => s"[$idx]${helper(indent, child)}"
        case Multiple(children) =>
          children.foldLeft("") { (acc, child) =>
            s"${acc}\n${tab * indent}${helper(indent + 1, child)}"
          }
      }
      s"/${helper(1, error)}"
    }
  }

  type Result[T] = Either[Error, T]

  def fail(error: Error) = Left(error)
  def fail(cause: String) = Left(Error.Failure(cause))
  def pass[T](value: T) = Right(value)

  def oneOf[T](fns: (JValue => Result[T])*)(value: JValue): Result[T] = {
    val (passes, fails) = fns.map(_.apply(value)).partition(_.isRight)
    passes match {
      case Seq(head) => head
      case Seq() => fail(Error.Multiple(fails.flatMap(_.swap.toSeq)))
      case _ => fail(Error.Failure(s"value matches more than one of ${fns}"))
    }
  }

  def map2[A, B, C](
    fn0: JValue => Result[A],
    fn1: JValue => Result[B],
    combiner: (A, B) => C)(value: JValue): Result[C] = {
    fn0(value) match {
      case Left(e) => fail(e)
      case Right(a) => fn1(value) match {
        case Left(e) => fail(e)
        case Right(b) => pass(combiner(a, b))
      }
    }
  }

  def map3[A, B, C, D](
    fn0: JValue => Result[A],
    fn1: JValue => Result[B],
    fn2: JValue => Result[C],
    combiner: (A, B, C) => D)(value: JValue): Result[D] = {
    fn0(value) match {
      case Left(e) => fail(e)
      case Right(a) => fn1(value) match {
        case Left(e) => fail(e)
        case Right(b) => fn2(value) match {
          case Left(e) => fail(e)
          case Right(c) => pass(combiner(a, b, c))
        }
      }
    }
  }

  def objMap[T](fn: (String, JValue) => Result[T])(value: JValue): Result[Seq[T]] = value match {
    case JObject(fields) =>
      val (passes: Seq[Result[T]], fails: Seq[Result[T]]) = fields.map({ case (field, value) =>
        fn(field, value).swap.map(Error.Field(field, _)).swap
      }).partition(_.isRight)

      if (fails.nonEmpty) {
        fail(Error.Multiple(fails.flatMap(_.swap.toSeq)))
      } else {
        pass(passes.flatMap(_.toSeq))
      }
    case _ => fail(Error.Failure(s"not a JObject"))
  }

  def field[T](name: String, nextDecoder: JValue => Result[T])(value: JValue): Result[T] = value match {
    case JObject(fields) => fields.find(_._1 == name) match {
      case Some((_, selected)) =>
        nextDecoder(selected).swap.map(Error.Field(name, _)).swap
      case None => fail(s"missing field '$name'")
    }
    case _ => fail(Error.Failure(s"not a JObject"))
  }

  def fieldOption[T](name: String, someDecoder: JValue => Result[T])(value: JValue): Result[Option[T]] = value match {
    case JObject(fields) => fields.find(_._1 == name) match {
      case Some((_, selected)) =>
        someDecoder(selected).map(value => Some(value)).swap.map(Error.Field(name, _)).swap
      case None => fail(s"missing field '$name'")
    }
    case _ => fail(Error.Failure(s"not a JObject"))
  }

  def index[T](idx: Int, nextDecoder: JValue => Result[T])(value: JValue): Result[T] = value match {
    case JArray(elements) => elements.lift(idx) match {
      case Some(selected) =>
        nextDecoder(selected).swap.map(Error.Index(idx, _)).swap
      case None => fail(s"index $idx is out of bounds")
    }
    case _ => fail(s"not a JArray")
  }

  def string[T](fn: String => Result[T])(value: JValue): Result[T] = value match {
    case JString(str) => fn(str)
    case _ => fail("not a JString")
  }

  def string(value: JValue): Result[String] = string(pass(_: String))(value)

  def integer[T](fn: BigInt => Result[T])(value: JValue): Result[T] = value match {
    case JInt(num) => fn(num)
    case _ => fail("not a JDouble")
  }

  def integer(value: JValue): Result[BigInt] = integer(pass(_: BigInt))(value)

  def boolean[T](fn: Boolean => Result[T])(value: JValue): Result[T] = value match {
    case JBool(bool) => fn(bool)
    case _ => fail("not a JBool")
  }

  def boolean(value: JValue): Result[Boolean] = boolean(pass(_: Boolean))(value)
}
