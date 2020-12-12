import better.files._

object Day12B extends App {

  sealed trait Instruction

  case class North(value: Int) extends Instruction
  case class East(value: Int) extends Instruction
  case class South(value: Int) extends Instruction
  case class West(value: Int) extends Instruction

  case class Position(north : North, east: East) {
    def move(instruction: Instruction) : Position = instruction match {
      case North(v) => copy(north = North(north.value + v))
      case South(v) => copy(north = North(north.value - v))
      case East(v) => copy(east = East(east.value + v))
      case West(v) => copy(east = East(east.value - v))
    }

    def rotateWaypointRight(orientationInDegrees : Int) : Position =
      orientationInDegrees % 360 % -360 match {
        case 0 => this
        case 90 | -270 => Position(North(-east.value), East(north.value))
        case 180 | -180 => Position(North(-north.value), East(-east.value))
        case 270 | -90 => Position(North(east.value), East(-north.value))
      }

    def rotateWaypointLeft(orientationInDegrees : Int) : Position =
      rotateWaypointRight(-orientationInDegrees)

    def manhattanDistance : Int =
      north.value.abs + east.value.abs

    def moveToWaypoint(waypointPosition : Position, numTimes : Int) : Position =
      Position(
        North(this.north.value + (waypointPosition.north.value * numTimes)),
        East(this.east.value + (waypointPosition.east.value * numTimes))
      )
  }

  val f = file"./files/day12/input"
  val (shipPosition, waypointPosition) =
    f.lines.foldLeft((Position(North(0), East(0)), Position(North(1), East(10)))){
    case ((shipPosition, waypointPosition), line) =>
      val (instruction, valueString) = line.splitAt(1)
      val value = valueString.toInt
      instruction match {
        case "E" => (shipPosition, waypointPosition.move(East(value)))
        case "S" => (shipPosition, waypointPosition.move(South(value)))
        case "W" => (shipPosition, waypointPosition.move(West(value)))
        case "N" => (shipPosition, waypointPosition.move(North(value)))
        case "L" => (shipPosition, waypointPosition.rotateWaypointLeft(value))
        case "R" => (shipPosition, waypointPosition.rotateWaypointRight(value))
        case "F" => (shipPosition.moveToWaypoint(waypointPosition, value), waypointPosition)
      }
  }

  println("Day12B:")
  println(shipPosition.manhattanDistance)
}
