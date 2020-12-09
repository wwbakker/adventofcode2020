import better.files._

object Day9A extends App {
  val f = file"./files/day9/input"
  val numbers = f.lines.map(_.toLong)
  val sliderLength = 25

  def containsSum(number: Long, list: Seq[Long]): Boolean = {
    val listWithIndex = list.zipWithIndex
    for {
      (a, i1) <- listWithIndex
      (b, _) <- listWithIndex.drop(i1 + 1) if a + b == number
    } yield b
  }.nonEmpty

  val result : Option[Long] =
    numbers
      .toList
      .sliding(sliderLength + 1)
      .map(_.reverse)
      .find {
        case number :: list =>
          !containsSum(number, list)
      }.flatMap(_.headOption)

  println("Day 9A")
  println(result)

}
