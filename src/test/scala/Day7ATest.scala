import org.scalatest.flatspec.AnyFlatSpec
import atto._
import Atto._
import Day7Model.{Bag, BagRule, Content}
import atto.ParseResult.Done
import cats.implicits._

class Day7ATest extends AnyFlatSpec {

  "content description" should "parse correctly" in {
    assert(Day7Model.contentDescription.parse("1 dark teal bag.") == Done(".", Content(1, Bag("dark", "teal"))))
  }

  "a rule" should "parse correctly with contents" in {
    assert(Day7Model.bagRule.parse("light red bags contain 1 bright white bag, 2 muted yellow bags.").done == Done("",BagRule(Bag("light","red"),List(Content(1,Bag("bright","white")), Content(2,Bag("muted","yellow"))))))
  }

  "a rule" should "parse correctly" in {
    assert(Day7Model.bagRule.parse("faded blue bags contain no other bags.").done == Done("",BagRule(Bag("faded","blue"),Nil)))
  }
}
