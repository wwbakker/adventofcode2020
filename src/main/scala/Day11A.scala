import better.files._

import scala.annotation.tailrec

object Day11A extends App  {

  sealed trait SeatLocation
  case object Floor extends SeatLocation
  case object EmptySeat extends SeatLocation
  case object OccupiedSeat extends SeatLocation
  type Coordinates = (Int, Int)

  val f = file"./files/day11/input"

  def parseRow(s : String) : Seq[(Int, SeatLocation)] =
    s.map[SeatLocation]{
      case '#' => OccupiedSeat
      case 'L' => EmptySeat
      case '.' => Floor
    }.zipWithIndex.map(_.swap)

  val initialSeatPlan : Map[Coordinates, SeatLocation] =
    f.lines
      .map(parseRow)
      .zipWithIndex.map(_.swap)
      .flatMap{ case (y, row) =>
        row.map{ case (x, seat) =>
          (x, y) -> seat
        }
      }.toMap.withDefaultValue(EmptySeat)

  def numberOfOccupiedAdjacentSeats(seatPlan : Map[Coordinates, SeatLocation],
                                    seatCoordinates : Coordinates) : Int = {
    val (x, y) = seatCoordinates
    val adjacentSeats = Seq(
      (x - 1, y - 1),
      (x, y - 1),
      (x + 1, y - 1),
      (x - 1, y),
      // not itself
      (x + 1, y),
      (x - 1, y + 1),
      (x, y + 1),
      (x + 1, y + 1),
    ).map(seatPlan)

    adjacentSeats.count(_ == OccupiedSeat)
  }


  def applyRules(seatPlan : Map[Coordinates, SeatLocation]) : Map[Coordinates, SeatLocation] =
    seatPlan.keys.map{ coordinates =>
      val nooas = numberOfOccupiedAdjacentSeats(seatPlan, coordinates)
      val newSeat = seatPlan(coordinates) match {
        case EmptySeat if nooas == 0 =>
          OccupiedSeat
        case OccupiedSeat if nooas >= 4 =>
          EmptySeat
        case seat => seat
      }
      coordinates -> newSeat
    }.toMap.withDefaultValue(EmptySeat)

  @tailrec
  def runModel(seatPlan : Map[Coordinates, SeatLocation]) : Map[Coordinates, SeatLocation] = {
    val newSeatPlan = applyRules(seatPlan)
    println(seatPlanToString(newSeatPlan))
    if (newSeatPlan == seatPlan)
      seatPlan
    else
      runModel(newSeatPlan)
  }

  def seatPlanToString(seatPlan : Map[Coordinates, SeatLocation]) : String = {
    val width = seatPlan.keys.map(_._1).max
    val height = seatPlan.keys.map(_._2).max
    val seats = for {
      x <- 0 to width
      y <- 0 to height
    } yield (y, seatPlan(x, y))
    seats.groupBy(_._1).map { case (_, seats) =>
      seats.map(_._2).map(seatLocationToChar).mkString + System.lineSeparator()
    }
  }.mkString

  def seatLocationToChar(seatLocation: SeatLocation) : Char = seatLocation match {
    case OccupiedSeat => '#'
    case EmptySeat => 'L'
    case Floor => '.'
  }


  println("Day11A:")
  println(runModel(initialSeatPlan).values.count(_ == OccupiedSeat))
}
