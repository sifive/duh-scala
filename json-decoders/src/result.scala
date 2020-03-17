package duh

sealed trait Result[+P, +F] {
  def isPass: Boolean = this match {
    case p: Pass[_] => true
    case f: Fail[_] => false
  }
  def isFail: Boolean = this match {
    case p: Pass[_] => false
    case f: Fail[_] => true
  }

  def swap: Result[F, P] = this match {
    case Pass(value) => Fail(value)
    case Fail(value) => Pass(value)
  }
  def getOrElse[Default >: P](default: => Default): Default = this match {
    case Fail(value) => default
    case Pass(value) => value
  }
  def orElse[Default >: P](default: => Default): Result[Default, F] = this match {
    case Fail(_) => Pass(default)
    case p: Pass[_] => this
  }
  def toOption: Option[P] = this match {
    case f: Fail[_] => None
    case Pass(value) => Some(value)
  }
  def foreach(fn: => Unit): Unit = this match {
    case f: Fail[_] => Unit
    case Pass(value) => Some(value)
  }
  def map[NextP](fn: P => NextP): Result[NextP, F] = this match {
    case p@ Pass(value) => Pass(fn(value))
    case f: Fail[_] => f
  }
  def flatMap[NextP, NextF >: F](fn: P => Result[NextP, NextF]): Result[NextP, NextF] = this match {
    case p@ Pass(value) => fn(value)
    case f: Fail[_] => f
  }
}

object Result {
  def cond[P, F](test: Boolean, pass: => P, fail: => F): Result[P, F] =
    if (test) Pass(pass) else Fail(fail)
}

final class Pass[+P] private (val value: P) extends Result[P, Nothing]

object Pass {
  def apply[T](pass: T): Pass[T] =
    new Pass[T](pass)

  def unapply[T](pass: Pass[T]): Option[T] = Some(pass.value)
}

final class Fail[+F] private (val value: F) extends Result[Nothing, F]

object Fail {
  def apply[T](fail: T): Fail[T] =
    new Fail[T](fail)

  def unapply[T](fail: Fail[T]): Option[T] = Some(fail.value)
}
