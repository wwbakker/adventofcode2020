import better.files._
import atto._
import Atto._
import cats.implicits.catsSyntaxSemigroup

object Day16A extends App {
  val f = file"./files/day16/input"

  case class Validation(fieldName: String, ranges: Seq[Range]) {
    def isWithinRange(v: Int): Boolean =
      ranges.exists(_.contains(v))
  }

  case class Ticket(numbers: Seq[Int])

  val rangeParser: Parser[Range] = for {
    from <- int <~ string("-")
    to <- int
  } yield Range.inclusive(from, to)

  val validationParser: Parser[Validation] = for {
    fieldName <- stringOf1(letter |+| whitespace) <~ string(":") <~ whitespace
    ranges <- sepBy(rangeParser, string(" or "))
  } yield Validation(fieldName, ranges)

  def parseValidations(validationsString: String): Seq[Validation] =
    validationsString
      .split(System.lineSeparator())
      .map(validationParser.parse)
      .map(_.done.option.get)

  def parseTicket(ticketString: String): Ticket =
    Ticket(
      ticketString
        .split(',')
        .map(_.toInt)
    )


  def parseYourTicket(yourTicketString: String) : Ticket =
    parseTicket(yourTicketString.split(System.lineSeparator()).last)


  def parseNearbyTickets(nearbyTicketsStrings: String): Seq[Ticket] =
    nearbyTicketsStrings.split(System.lineSeparator())
      .tail
      .map(parseTicket)


  val (validations, yourTicket, nearbyTickets) =
    f.contentAsString.split(System.lineSeparator() + System.lineSeparator()).toList match {
      case validationsString :: yourTicketString :: nearbyTicketsString :: Nil =>
        (parseValidations(validationsString), parseYourTicket(yourTicketString),
          parseNearbyTickets(nearbyTicketsString))
    }

  val ticketScanningErrorRate =
    nearbyTickets
      .flatMap(_.numbers)
      .filter(n => !validations.exists(_.isWithinRange(n)))
      .sum

  println("Day16A")
  println(ticketScanningErrorRate)


}
