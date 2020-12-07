import atto._, Atto._

object Day7Model {
  case class Bag(colorModifier: String, color : String)
  case class Content(number : Int, bag : Bag)
  case class BagRule(selfBag: Bag, contents : List[Content]) {
    def contains(bag : Bag)(implicit rs : Seq[BagRule]) : Boolean =
      contents.map(_.bag).exists { b =>
        if (b == bag) {
          // directly (in the contents)
          true
        } else {
          // indirectly (in contents of contents)
          BagRule.findByBag(b).contains(bag)
        }
      }

    def numberOfBagsInside(implicit rs : Seq[BagRule]) : Int =
      contents
        // number of bags * (itself + its contents)
        .map(c => c.number * (1 + BagRule.findByBag(c.bag).numberOfBagsInside))
        .sum
  }
  object BagRule {
    def findByBag(bag : Bag)(implicit rs : Seq[BagRule]) : BagRule =
      rs.find(_.selfBag == bag).get
  }

  val bagDescription: Parser[Bag] = for {
    colorModifier <- stringOf1(letter) <~ whitespace
    color         <- stringOf1(letter) <~ whitespace ~ string("bag") ~ opt(char('s'))
  } yield Bag(colorModifier, color)


  val contentDescription : Parser[Content] = for {
    number        <- int <~ whitespace
    bag           <- bagDescription
  } yield Content(number, bag)


  val bagRule : Parser[BagRule] = for {
    selfBag       <- bagDescription <~ string(" contain ")
    contents      <- either(string("no other bags"), sepBy(contentDescription, string(", "))) <~ char('.')
  } yield BagRule(selfBag, contents.getOrElse(Nil))

}