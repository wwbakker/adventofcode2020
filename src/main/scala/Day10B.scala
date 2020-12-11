import better.files._

object Day10B extends App {
  val f = file"./files/day10/input"

  val adapters: Seq[Int] = f.lines.map(_.toInt).toSeq
  val output = adapters.max + 3
  val steps = (0 +: adapters).toSet

  def numberOfWays(n : Int) : Int = {
    if (n == 0) 1
    else
      (if (steps.contains(n - 1)) numberOfWays(n - 1) else 0) +
        (if (steps.contains(n - 2)) numberOfWays(n - 2) else 0) +
        (if (steps.contains(n - 3)) numberOfWays(n - 3) else 0)
  }


  val now = numberOfWays(output)

  println("Day10B:")
  println(now)

}
