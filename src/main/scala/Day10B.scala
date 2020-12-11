import better.files._

import scala.annotation.tailrec

object Day10B extends App {
  val f = file"./files/day10/example2"

  val adapters: Iterable[Int] = f.lines.map(_.toInt).toSeq
  val steps = (Seq(adapters.max) ++ adapters).toSet

  @tailrec
  def optimalSet(currentSet : Set[Int], currentE : Int) : Set[Int] =
    if (steps.contains(currentE + 3))
      optimalSet(currentSet.incl(currentE + 3), currentE + 3)
    else if (steps.contains(currentE + 2))
      optimalSet(currentSet.incl(currentE + 2), currentE + 2)
    else if (steps.contains(currentE + 1))
      optimalSet(currentSet.incl(currentE + 1), currentE + 1)
    else if (steps.max > currentE)
      throw new IllegalArgumentException("Cannot reach the highest adapter")
    else
      currentSet

  val difference = steps.diff(optimalSet(Set.empty, 0))
  val variations = Math.pow(2, difference.size)

  println("Day10a:")
  println(difference.toSeq.sorted)
  println(variations)

//  val count: Count = adapters.foldLeft(Count(0,0,0))((count, e) =>
//    e - count.previousJoltage match {
//      case 1 => Count(e, count.oneDifference + 1, count.threeDifference)
//      case 0 | 2 => Count(e, count.oneDifference, count.threeDifference)
//      case 3 => Count(e, count.oneDifference, count.threeDifference + 1)
//      case _ => throw new IllegalArgumentException("Too much difference")
//    }
//  )



}
