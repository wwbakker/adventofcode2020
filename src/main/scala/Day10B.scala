import better.files._

object Day10B extends App {
  val f = file"./files/day10/input"

  val adapters: Seq[Int] = f.lines.map(_.toInt).toSeq
  val output = adapters.max + 3
  val steps = (adapters :+ output).sorted
  val numberOfWays : Map[Int, Long] = steps.foldLeft(Map(0 -> 1L))((previousMap, n) =>
    previousMap + (n ->
      (previousMap.getOrElse(n - 1, 0L) +
        previousMap.getOrElse(n - 2, 0L) +
        previousMap.getOrElse(n - 3, 0L)) )
  )

  println("Day10B:")
  println(numberOfWays.get(output))

}
