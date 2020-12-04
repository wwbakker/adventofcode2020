import better.files._

object Day1B extends App {

  def sumTo2020() : Option[Int] = {
    val f = file"./files/day1/input"
    val entries : Iterable[(Int, Int)] = f.lines.map(_.toInt).zipWithIndex
    for {
      (a, i1) <- entries
      (b, i2) <- entries.drop(i1 + 1)
      (c, _) <- entries.drop(i2 + 1) if a + b + c == 2020
    } yield a * b * c
  }.headOption


  println("Day 1B")
  println(sumTo2020())

}
