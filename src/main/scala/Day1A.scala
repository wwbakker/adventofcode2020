import better.files._

object Day1A extends App {

  def sumTo2020() : Option[Int] = {
    val f = file"./files/day1/input"
    val entries : Iterable[Int] = f.lines.map(_.toInt)
    for {
      a <- entries
      b <- entries if a + b == 2020
    } yield a * b
  }.headOption


  println("Day 1A")
  println(sumTo2020())

}
