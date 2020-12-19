import better.files._

import scala.annotation.tailrec

object Day15A extends App {
  val f = file"./files/day15/input"

  val startingNumbers: List[Long] = f.lines.head.split(",").map(_.toLong).reverse.toList
  val startingNumbersLength = startingNumbers.length

  case class Value(index: Int, v: Long)

  def nextValue(previousValuesReversed: List[Long]): Long = previousValuesReversed match {
    case x :: xs =>
      val howManyTurnsApart = xs.indexOf(x) + 1
      if (howManyTurnsApart == 0) // never spoken before
        0L
      else
        howManyTurnsApart
  }

  @tailrec
  def determineNumberAtIndex(indexToKnow: Int,
                             previousValuesReversed: List[Long]): Long = {
    if (previousValuesReversed.length == indexToKnow)
      previousValuesReversed.head
    else
      determineNumberAtIndex(indexToKnow, nextValue(previousValuesReversed) :: previousValuesReversed)
  }

  println("Day15A:")
  println(determineNumberAtIndex(2020, startingNumbers))

}
