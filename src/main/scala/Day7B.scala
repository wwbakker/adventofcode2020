import better.files._

import atto._, Atto._

object Day7B extends App {
  import Day7Model._
  val f = file"./files/day7/input"


  val rules: Seq[BagRule] =
    f.lines
    .map(bagRule.parse)
    .map(_.done.option.get)
      .toSeq

  val shinyGoldBagRule = rules.find(_.selfBag == Bag("shiny", "gold")).get

  println("Day7B:")
  println(shinyGoldBagRule.numberOfBagsInside(rules))

}

