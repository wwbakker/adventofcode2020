import better.files._

import scala.annotation.tailrec

object Day5A extends App {
  val f = file"./files/day5/input"
  val numRows = 128
  val numColumns = 8

  sealed trait Instruction
  case object Lower extends Instruction
  case object Higher extends Instruction

  @tailrec
  def determineIndex(lowerBoundIndex: Int, length: Int, pendingInstructions: List[Instruction]): Int =
    pendingInstructions match {
      case Nil if length != 1 => throw new IllegalStateException("Something went wrong, not enough instruction")
      case _ :: _ if length == 1 => throw new IllegalStateException("Something went wrong, too many instructions")
      case Nil => lowerBoundIndex
      case Lower :: xs => determineIndex(lowerBoundIndex, length / 2, xs)
      case Higher :: xs => determineIndex(lowerBoundIndex + (length / 2), length / 2, xs)
    }

  case class SeatInstructions(rowInstructions: List[Instruction], columnInstructions: List[Instruction]) {
    lazy val rowIndex: Int = determineIndex(0, numRows, rowInstructions)
    lazy val columnIndex: Int = determineIndex(0, numColumns, columnInstructions)

    def seatId : Int =
       rowIndex * 8 + columnIndex
  }

  object SeatInstructions {

    def parseRowInstruction(c : Char) : Instruction = c match {
      case 'F' => Lower
      case 'B' => Higher
      case _ => throw new IllegalArgumentException("Unexpected character when parsing row")
    }

    def parseColumnInstruction(c : Char) : Instruction = c match {
      case 'L' => Lower
      case 'R' => Higher
      case _ => throw new IllegalArgumentException("Unexpected character when parsing column")
    }

    def parse(s: String): SeatInstructions =
      SeatInstructions(
        rowInstructions = s.substring(0, 7).map(parseRowInstruction).toList,
        columnInstructions = s.substring(7, 10).map(parseColumnInstruction).toList
      )
  }

  val maxSeatId = f.lines.map(SeatInstructions.parse).map(_.seatId).max

  println("Day5A:")
  println(s"Max seat id: $maxSeatId")
}
