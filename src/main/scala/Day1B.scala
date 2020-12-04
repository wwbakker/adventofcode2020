import better.files._

object Day1B extends App {

  def sumTo2020() : Option[Int] = {
    val f = file"./files/day1/input"
    val entries : Iterable[(Int, Int)] = f.lines.map(_.toInt).zipWithIndex
    for {
      (a, i1) <- entries
      (b, i2) <- entries
      (c, i3) <- entries if a + b + c == 2020 && i1 != i2 && i2 != i3 && i1 != i3
    } yield a * b * c
  }.headOption


  println("Day 1B")
  println(sumTo2020())

}
