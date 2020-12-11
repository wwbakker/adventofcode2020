import better.files._

object Day10A extends App {
  val f = file"./files/day10/input"

  case class Count(previousJoltage : Int, oneDifference : Int, threeDifference : Int) {
    def result : Int = oneDifference * (threeDifference + 1)
  }

  val adapters: Iterable[Int] = f.lines.map(_.toInt).toSeq.sorted
  val count: Count = adapters.foldLeft(Count(0,0,0))((count, e) =>
    e - count.previousJoltage match {
      case 1 => Count(e, count.oneDifference + 1, count.threeDifference)
      case 0 | 2 => Count(e, count.oneDifference, count.threeDifference)
      case 3 => Count(e, count.oneDifference, count.threeDifference + 1)
      case _ => throw new IllegalArgumentException("Too much difference")
    }
  )

  println("Day10a:")
  println(count)
  println(count.result)

}
