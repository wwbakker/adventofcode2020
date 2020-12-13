import better.files._

object Day13A extends App{
  val f = file"./files/day13/input"

  val (earliestTimeToDepart : Int, busIds : Seq[Int]) = f.lines.toList match {
    case firstLine :: secondLine :: Nil =>
      (firstLine.toInt, secondLine.split(",").filterNot(_=="x").map(_.toInt).toSeq)
  }

  val busIdWithMinutesToWait : Seq[(Int, Int)] =
    busIds
      .map(busId => (busId, busId - (earliestTimeToDepart % busId) ))
      .sortBy(_._2)

  println("Day13A:")
  println(busIdWithMinutesToWait)
  println(busIdWithMinutesToWait.headOption.map{ case (busId, minutesToWait) => busId * minutesToWait})

}
