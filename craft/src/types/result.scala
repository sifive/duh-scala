package duh.scala.types

sealed trait Result[+PassType]

sealed trait Fail extends Result[Nothing] {
  def message: String
}
