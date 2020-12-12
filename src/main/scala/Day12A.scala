import better.files._

object Day12A extends App {

  sealed trait Instruction

  case class North(value: Int) extends Instruction

  case class East(value: Int) extends Instruction

  case class South(value: Int) extends Instruction

  case class West(value: Int) extends Instruction

  object Instruction {
    def fromDegrees(degrees: Long, value: Int): Instruction =
      degrees % 360 % -360 match {
        case 0 => East(value)
        case 90 | -270 => South(value)
        case 180 | -180 => West(value)
        case 270 | -90 => North(value)
      }
  }

  case class Position(north : North, east: East) {
    def apply(instruction: Instruction) : Position = instruction match {
      case North(v) => copy(north = North(north.value + v))
      case South(v) => copy(north = North(north.value - v))
      case East(v) => copy(east = East(east.value + v))
      case West(v) => copy(east = East(east.value - v))
    }
    def manhattanDistance : Int =
      north.value.abs + east.value.abs
  }

  val f = file"./files/day12/input"
  val (finalOrientation, instructions): (Long, List[Instruction]) =
    f.lines.foldLeft((0L, List.empty[Instruction])) {
      case ((orientationInDegrees, previousInstructions), line) =>
        val (instruction, valueString) = line.splitAt(1)
        val value = valueString.toInt
        instruction match {
          case "E" => (orientationInDegrees, East(value) :: previousInstructions)
          case "S" => (orientationInDegrees, South(value) :: previousInstructions)
          case "W" => (orientationInDegrees, West(value) :: previousInstructions)
          case "N" => (orientationInDegrees, North(value) :: previousInstructions)
          case "L" => (orientationInDegrees - value, previousInstructions)
          case "R" => (orientationInDegrees + value, previousInstructions)
          case "F" => (orientationInDegrees, Instruction.fromDegrees(orientationInDegrees, value) :: previousInstructions)
        }
    }

  val position: Position =
    instructions.foldLeft(Position(North(0), East(0)))(_.apply(_))

  println("Day12A:")
  println(finalOrientation)
  println(instructions.reverse.map(_.toString).mkString("\n"))
  println(position)
  println(position.manhattanDistance)

}
