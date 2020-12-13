import better.files._

import scala.collection.immutable.NumericRange

object Day13B extends App{
  val f = file"./files/day13/example"

  type Index = Int

  val busIds: Seq[(Index, NumericRange.Inclusive[Index])] = f.lines.toList match {
    case _ :: secondLine :: Nil =>
      secondLine.split(",")
        .zipWithIndex
        .filterNot(_._1=="x")
        .map{case(busId, index) =>
          (index, NumericRange.inclusive(0, Int.MaxValue, busId.toInt))}
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
