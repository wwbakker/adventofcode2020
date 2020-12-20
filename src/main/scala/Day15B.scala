import better.files._

import scala.annotation.tailrec

object Day15B extends App {
  val f = file"./files/day15/example"

  val startingNumbers: Array[Long] = f.lines.head.split(",").map(_.toLong)
  val startingNumbersLength = startingNumbers.length
  val indexToKnow = 300000000
  val percentage = (indexToKnow / 100).intValue

  val numbers : Array[Long] = Array.fill(indexToKnow + 1)(0L)
  startingNumbers.copyToArray(numbers)


//  case class Value(index: Int, v: Long)
//
//  def nextValue(previousValuesReversed: List[Long]): Long = previousValuesReversed match {
//    case x :: xs =>
//      val howManyTurnsApart = xs.indexOf(x) + 1
//      if (howManyTurnsApart == 0) // never spoken before
//        0L
//      else
//        howManyTurnsApart
//  }
//
  @tailrec
  def nextValue(indexToConsider: Int, indexOfPreviousValue : Int, previousValue : Long) : Long =
    if (indexToConsider == -1)
      0L
    else if (numbers(indexToConsider) == previousValue)
      indexOfPreviousValue - indexToConsider
    else
      nextValue(indexToConsider - 1, indexOfPreviousValue, previousValue)


  @tailrec
  def determineNumberAtIndex(currentIndex : Int) : Unit = {
    if ((currentIndex % percentage) == 0) {
      println(currentIndex / percentage)
    }

    if (currentIndex == indexToKnow)
      numbers(currentIndex)
    else {
      numbers(currentIndex) = nextValue(currentIndex - 2, currentIndex - 1, numbers(currentIndex - 1))
      determineNumberAtIndex(currentIndex + 1)
    }
}
  determineNumberAtIndex(startingNumbersLength)

  println("Day15B:")
  println(numbers(indexToKnow - 1))

}
