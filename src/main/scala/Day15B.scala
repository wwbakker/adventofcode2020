import better.files._

import scala.annotation.tailrec

object Day15B extends App {
  val f = file"./files/day15/input"

  val startingNumbers: Array[Int] = f.lines.head.split(",").map(_.toInt)
  val startingNumbersLength = startingNumbers.length
  val turnToKnow = 30000000

  val turnValueWasLastSpoken: Array[Int] = Array.fill(turnToKnow + 1)(-1)
  startingNumbers.toSeq.zipWithIndex.reverse.tail.reverse.foreach { case (number, index) => turnValueWasLastSpoken(number) = index + 1 }

  def determineCurrentValue(previousTurn: Int, previousValue: Int): Int =
    if (turnValueWasLastSpoken(previousValue) == -1)
      0
    else
      previousTurn - turnValueWasLastSpoken(previousValue)

  @tailrec
  def determineValueAtTurn(currentTurn: Int, previousValue: Int): Int = {
    val previousTurn = currentTurn - 1
    val currentValue = determineCurrentValue(previousTurn, previousValue)

    if (currentTurn == turnToKnow)
      currentValue
    else {
      turnValueWasLastSpoken(previousValue) = previousTurn
      determineValueAtTurn(currentTurn + 1, currentValue)
    }
  }

  println("Day15B:")
  println(determineValueAtTurn(startingNumbersLength + 1, startingNumbers.last))

}
