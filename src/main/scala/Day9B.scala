import better.files._

import scala.annotation.tailrec

object Day9B extends App {
  val f = file"./files/day9/input"
  val numbers = f.lines.map(_.toLong).toSeq
  val resultOfDay9A = 1212510616L


  @tailrec
  def subList(list : Seq[Long]) : Option[Seq[Long]] = {
    val sum = list.sum
    if (sum > resultOfDay9A) {
      subList(list.take(list.length - 1))
    } else if (sum == resultOfDay9A)
      Some(list)
    else
      None
  }

  @tailrec
  def findContiguousSet(from : Seq[Long]) : Option[Seq[Long]] =
    subList(from) match {
      case Some(result) => Some(result)
      case None if from.nonEmpty => findContiguousSet(from.tail)
      case None => None
    }

  val resultOption = findContiguousSet(numbers)

  println("Day 9A")
  println(resultOption)
  println(resultOption.map(resultList => resultList.min + resultList.max))


}
