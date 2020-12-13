import better.files._

object Day13B extends App{
  val f = file"./files/day13/example"

  type Index = Int

  case class LongRange(step : Long) {
    def find(func : Long => Boolean) : Option[Long] =
      Iterator.iterate(0L)(_+step).find(func)
    def contains(v : Long) : Boolean =
      v % step == 0

  }

  val busIds: Seq[(Index, LongRange)] = f.lines.toList match {
    case _ :: secondLine :: Nil =>
      secondLine.split(",")
        .zipWithIndex
        .filterNot(_._1=="x")
        .map{case(busId, index) =>
          (index, LongRange(busId.toLong))}
//        .sortBy(_._2.step).reverse
        .toList
  }

  val t = busIds match {
    case (_, firstBusRange) :: otherBusses =>
      firstBusRange.find(time =>
        otherBusses.forall{ case (otherBusIndex, otherBusRange) =>
          otherBusRange.contains(time + otherBusIndex)
        }
      )
  }

  println("Day13B:")
  println(t)
}
