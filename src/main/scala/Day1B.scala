import better.files._

object Day1B extends App {

  def sumTo2020() : Option[Int] = {
    val f = file"./files/day1/input"
    val entries : Iterable[Int] = f.lines.map(_.toInt)
    for {
      a <- entries
      b <- entries
      c <- entries if a + b + c == 2020
    } yield a * b * c
  }.headOption


  println("Day 1B")
  println(sumTo2020())

}
