import better.files._

import atto._, Atto._

object Day7A extends App {
  import Day7Model._
  val f = file"./files/day7/input"


  val rules: Seq[BagRule] =
    f.lines
    .map(bagRule.parse)
    .map(_.done.option.get)
      .toSeq

  println("Day7A:")
  println(rules.count(_.contains(Bag("shiny", "gold"))(rules)))

}

