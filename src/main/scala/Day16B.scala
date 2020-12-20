import atto.Atto._
import atto._
import better.files._
import cats.implicits.catsSyntaxSemigroup

object Day16B extends App {
  val f = file"./files/day16/input"

  case class Field(fieldName: String, validRanges: Seq[Range]) {
    def valueIsValidForField(v: Int): Boolean =
      validRanges.exists(_.contains(v))

    def isValidForIndex(ticket: Ticket, index : Int) : Boolean =
      valueIsValidForField(ticket.numbers(index))
  }

  case class Ticket(numbers: Seq[Int]) {
    def isIllegal(fields: Seq[Field]) : Boolean =
      numbers.exists(number =>
        !fields.exists(_.valueIsValidForField(number))
      )
  }

  val rangeParser: Parser[Range] = for {
    from <- int <~ string("-")
    to <- int
  } yield Range.inclusive(from, to)

  val validationParser: Parser[Field] = for {
    fieldName <- stringOf1(letter |+| whitespace) <~ string(":") <~ whitespace
    ranges <- sepBy(rangeParser, string(" or "))
  } yield Field(fieldName, ranges)

  def parseValidations(validationsString: String): Seq[Field] =
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


  val (fields, yourTicket, nearbyTickets) =
    f.contentAsString.split(System.lineSeparator() + System.lineSeparator()).toList match {
      case validationsString :: yourTicketString :: nearbyTicketsString :: Nil =>
        (parseValidations(validationsString), parseYourTicket(yourTicketString),
          parseNearbyTickets(nearbyTicketsString))
    }



  def indexOfField(tickets : Seq[Ticket], field: Field) : Option[Int] =
    yourTicket.numbers.indices.find(index =>
      tickets.forall(ticket =>
          field.isValidForIndex(ticket, index)))


  val validTickets =
    nearbyTickets
      .filterNot(_.isIllegal(fields))

  println("Day16B")
  //for example:
  //val fieldWithCorrectIndex: Seq[(Field, Option[Int])] =
  //  fields
  //    .map(f => f -> indexOfField(f))
  //println(fieldWithCorrectIndex.map{case (f, i) => s"${f.fieldName}: ${yourTicket.numbers(i.get)}"})

  // for input:
  val fieldWithCorrectIndex: Seq[(Field, Option[Int])] =
    fields
      .filter(_.fieldName.contains("departure"))
      .map(f => f -> indexOfField(validTickets, f))

  println(fieldWithCorrectIndex.map{case (f, i) => s"${f.fieldName}: ${yourTicket.numbers(i.get)}"})


}
