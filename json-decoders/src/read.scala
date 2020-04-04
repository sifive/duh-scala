package duh.json5

object Test extends App {
  def time[R](block: => R): (Double, R) = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    val timeMillis = (t1 - t0) / 1000000.0
    (timeMillis, result)
  }

  println(JValue.prettyPrint(parse(args(0))))
  println(time(parse(args(0)))._1)
  println(time(parse(args(0)))._1)
  println(time(parse(args(0)))._1)
  println(time(parse(args(0)))._1)
  println(time(parse(args(0)))._1)
  println(time(parse(args(0)))._1)
  println(time(parse(args(0)))._1)
  println(time(parse(args(0)))._1)
  println(time(parse(args(0)))._1)
  println(time(parse(args(0)))._1)
  println(time(parse(args(0)))._1)
}
