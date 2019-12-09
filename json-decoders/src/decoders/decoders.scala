// See LICENSE for license details.
package duh.scala

import org.json4s.JsonAST._
import scala.reflect.macros.whitebox
import scala.language.experimental.macros
import scala.language.dynamics

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
      s"error at /${helper(1, error)}"
    }
  }

  def get[T](result: Result[T]): T = result match {
    case Right(value) => value
    case Left(error) => throw new Exception(error.toString)
  }

  type Result[T] = Either[Error, T]
  type Decoder[T] = JValue => Result[T]

  def fail(error: Error): Result[Nothing] = Left(error)
  def fail(cause: String): Result[Nothing] = Left(Error.Failure(cause))
  def pass[T](value: T): Result[T] = Right(value)

  def oneOf[T](fns: Decoder[T]*)(value: JValue): Result[T] = {
    val (passes, fails) = fns.map(_.apply(value)).partition(_.isRight)
    passes match {
      case Seq(head) => head
      case Seq() => fail(Error.Multiple(fails.flatMap(_.swap.toSeq)))
      case _ => fail(Error.Failure(s"value matches more than one of ${fns}"))
    }
  }

  object rmapNamed extends Dynamic {
    def applyDynamicNamed(method: String)(rec: Any*): Any = macro MapMacros.rmapNamed
  }

  object jmapNamed extends Dynamic {
    def applyDynamicNamed[T](method: String)(args: (String, JValue => Any)*): JValue => Result[T] = macro MapMacros.jmapNamed[T]
  }

  object MapMacros
  class MapMacros(val c: whitebox.Context) {
    import c.universe._

    def jmapNamed[T](method: Tree)(args: Tree*)(implicit tag: WeakTypeTag[T]): Tree = {
      val q"${methodString: String}" = method
      if (methodString != "apply")
        c.abort(c.enclosingPosition, s"missing method '$methodString'")

      if (args.length > 1) {
        val jvalName = TermName(c.freshName("json"))
        val appliedArgs = args.map {
          case q"($name, $result)" => q"($name, $result($jvalName))"
        }
        val fn = q"${tag.tpe.typeSymbol.companion}.apply"
        q"""
        ($jvalName: org.json4s.JsonAST.JValue) => {
          ${makeMatches(fn, appliedArgs)}
        }
        """
      } else {
        args.head
      }
    }

    def rmapNamed(method: Tree)(rec: Tree*): Tree = {
      val q"${methodString: String}" = method
      if (methodString != "apply")
        c.abort(c.enclosingPosition, s"missing method '$methodString'")

      if (rec.length > 1) {
        val q"($name, $fn)" = rec.last
        val args = rec.dropRight(1)
        makeMatches(fn, args)
      } else {
        rec.head
      }
    }

    def makeMatches(fn: Tree, rec: Seq[Tree]): Tree = {
      def rmap(result: Tree, elem: TermName, value: Tree) = {
        val errorName = TermName(c.freshName("error"))
        q"""$result match {
          case scala.util.Left($errorName) => duh.scala.decoders.fail($errorName)
          case scala.util.Right($elem) => $value
        }"""
      }

      val nameArgResult = rec.map {
        case q"(${Literal(Constant(name: String))}, $result)" => (TermName(name), TermName(c.freshName("value")), result)
        case elem => c.abort(c.enclosingPosition, s"$elem has the wrong shape for a")
      }

      val lastValue = {
        val args = nameArgResult.map {
          case (name, arg, result) => q"""$name = $arg"""
        }
        q"""duh.scala.decoders.pass($fn(..$args))"""
      }

      nameArgResult.foldRight(lastValue: Tree) {
        case ((_, arg, result), acc) => rmap(result, arg, acc)
      }
    }
  }

  def map2[A, B, C](
    fn0: Decoder[A],
    fn1: Decoder[B],
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
    fn0: Decoder[A],
    fn1: Decoder[B],
    fn2: Decoder[C],
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

  def arrMap[T](fn: Decoder[T]): JValue => Result[Seq[T]] = {
    case JArray(elements) =>
      val (passes: Seq[Result[T]], fails: Seq[Result[T]]) =
        elements.zipWithIndex.map({ case (value, index) =>
          fn(value).swap.map(Error.Index(index, _)).swap
        }).partition(_.isRight)

      if (fails.nonEmpty) {
        fail(Error.Multiple(fails.flatMap(_.swap.toSeq)))
      } else {
        pass(passes.flatMap(_.toSeq))
      }
    case _ => fail(Error.Failure(s"not a JObject"))
  }

  def objMap[T](fn: (String, JValue) => Result[T]): JValue => Result[Seq[T]] =
    (value: JValue) => value match {
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

  def field[T](name: String, nextDecoder: Decoder[T]): JValue => Result[T] =
    (value: JValue) => value match {
      case JObject(fields) => fields.filter(_._1 == name) match {
        case List((_, selected)) =>
          nextDecoder(selected).swap.map(Error.Field(name, _)).swap
        case List() => fail(s"missing field '$name'")
        case _ => fail (s"more than one fields named '$name'")
      }
      case _ => fail(Error.Failure(s"not a JObject"))
    }

  def fieldOption[T](name: String, someDecoder: Decoder[T]): JValue => Result[Option[T]] = 
    (value: JValue) => value match {
      case JObject(fields) => fields.find(_._1 == name) match {
        case Some((_, selected)) =>
          someDecoder(selected).map(value => Some(value)).swap.map(Error.Field(name, _)).swap
        case None => pass(None)
      }
      case _ => fail(Error.Failure(s"not a JObject"))
    }

  def index[T](idx: Int, nextDecoder: Decoder[T]): JValue => Result[T] =
    (value: JValue) => value match {
      case JArray(elements) => elements.lift(idx) match {
        case Some(selected) =>
          nextDecoder(selected).swap.map(Error.Index(idx, _)).swap
        case None => fail(s"index $idx is out of bounds")
      }
      case _ => fail(s"not a JArray")
    }

  def hasType[T](required: String, nextDecoder: Decoder[T]): JValue => Result[T] =
    json => field("type", string((given: String) => given match {
      case given if given == required => nextDecoder(json)
      case _ => fail(s"type field must be '${required}'")
    }))(json)


  def string[T](fn: String => Result[T]): JValue => Result[T] = {
    case JString(str) => fn(str)
    case _ => fail("not a JString")
  }

  val string: JValue => Result[String] = string(pass(_: String))


  def integer[T](fn: BigInt => Result[T]): JValue => Result[T] = {
    case JInt(num) => fn(num)
    case _ => fail("not a JInt")
  }

  val integer: JValue => Result[BigInt] = integer(pass(_: BigInt))


  def double[T](fn: Double => Result[T]): JValue => Result[T] = {
    case JDouble(num) => fn(num)
    case _ => fail("not a JDouble")
  }

  val double: JValue => Result[Double] = double(pass(_: Double))


  def boolean[T](fn: Boolean => Result[T]): JValue => Result[T] = (value: JValue) => value match {
    case JBool(bool) => fn(bool)
    case _ => fail("not a JBool")
  }

  val boolean: JValue => Result[Boolean] = boolean(pass(_: Boolean))
}
