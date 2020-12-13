import better.files._

object Day13B extends App{
  val f = file"./files/day13/input"

  type Index = Int

  case class LongRange(step : Long, start : Long = 0L) {
    def find(func : Long => Boolean) : Option[Long] =
      Iterator.iterate(start)(_+step).find(func)
    def contains(v : Long) : Boolean =
      v % step == 0

    def skip(n : Long) : LongRange =
      LongRange(step, n - (n % step))
  }

  val busIds: Seq[(Index, LongRange)] = f.lines.toList match {
    case _ :: secondLine :: Nil =>
      secondLine.split(",")
        .zipWithIndex
        .filterNot(_._1=="x")
        .map{case(busId, index) =>
          (index, LongRange(busId.toLong))}
        .sortBy(_._2.step).reverse
        .toList
  }

  val t = busIds match {
    case (firstBusIndex, firstBusRange) :: otherBusses =>
      firstBusRange.skip(100000000000000L).find(time =>
          otherBusses.forall { case (otherBusIndex, otherBusRange) =>
            otherBusRange.contains(time - firstBusIndex + otherBusIndex)
          }
      ).map(_-firstBusIndex)
  }

  println("Day13B:")
  println(t)
}
