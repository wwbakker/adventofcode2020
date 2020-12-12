import better.files._

import scala.annotation.tailrec

object Day11B extends App {

  sealed trait SeatLocation

  case object Floor extends SeatLocation

  case object EmptySeat extends SeatLocation

  case object OccupiedSeat extends SeatLocation

  type Coordinates = (Int, Int)

  val f = file"./files/day11/input"

  def parseRow(s: String): Seq[(Int, SeatLocation)] =
    s.map[SeatLocation] {
      case '#' => OccupiedSeat
      case 'L' => EmptySeat
      case '.' => Floor
    }.zipWithIndex.map(_.swap)

  val initialSeatPlan: Map[Coordinates, SeatLocation] =
    f.lines
      .map(parseRow)
      .zipWithIndex.map(_.swap)
      .flatMap { case (y, row) =>
        row.map { case (x, seat) =>
          (x, y) -> seat
        }
      }.toMap.withDefaultValue(EmptySeat)

  @tailrec
  def firstSeatInDirection(seatPlan: Map[Coordinates, SeatLocation],
                           currentLocation: Coordinates,
                           direction: Coordinates): SeatLocation = {
    val newLocation =
      (currentLocation._1 + direction._1,
        currentLocation._2 + direction._2)

    if (!seatPlan.contains(newLocation)) {
      Floor
    } else if(seatPlan(newLocation) != Floor) {
      seatPlan(newLocation)
    } else {
      firstSeatInDirection(seatPlan, newLocation, direction)
    }
  }

  def numberOfOccupiedSeatsInAllDirections(seatPlan: Map[Coordinates, SeatLocation],
                                           seatCoordinates: Coordinates): Int = {

    Seq(
      (-1, -1),
      (0, -1),
      (+1, -1),
      (-1, 0),
      // not itself
      (+1, 0),
      (-1, 1),
      (0, +1),
      (+1, 1),
    )
      .map(firstSeatInDirection(seatPlan,seatCoordinates,_))
      .count(_ == OccupiedSeat)
  }


  def applyRules(seatPlan: Map[Coordinates, SeatLocation]): Map[Coordinates, SeatLocation] =
    seatPlan.keys.map { coordinates =>
      lazy val nooas = numberOfOccupiedSeatsInAllDirections(seatPlan, coordinates)
      val newSeat = seatPlan(coordinates) match {
        case EmptySeat if nooas == 0 =>
          OccupiedSeat
        case OccupiedSeat if nooas >= 5 =>
          EmptySeat
        case seat => seat
      }
      coordinates -> newSeat
    }.toMap.withDefaultValue(EmptySeat)

  @tailrec
  def runModel(seatPlan: Map[Coordinates, SeatLocation]): Map[Coordinates, SeatLocation] = {
    val newSeatPlan = applyRules(seatPlan)
    println(seatPlanToString(newSeatPlan))
    if (newSeatPlan == seatPlan)
      seatPlan
    else
      runModel(newSeatPlan)
  }

  def seatPlanToString(seatPlan: Map[Coordinates, SeatLocation]): String = {
    val width = seatPlan.keys.map(_._1).max
    val height = seatPlan.keys.map(_._2).max
    val seats = for {
      x <- 0 to width
      y <- 0 to height
    } yield (y, seatPlan(x, y))
    seats.groupBy(_._1).toSeq.sortBy(_._1).map { case (_, seats) =>
      seats.map(_._2).map(seatLocationToChar).mkString + System.lineSeparator()
    }
  }.mkString

  def seatLocationToChar(seatLocation: SeatLocation): Char = seatLocation match {
    case OccupiedSeat => '#'
    case EmptySeat => 'L'
    case Floor => '.'
  }


  println("Day11B:")
  println(seatPlanToString(initialSeatPlan))
  println(runModel(initialSeatPlan).values.count(_ == OccupiedSeat))
}
