import better.files._

import scala.annotation.tailrec

object Day8A extends App {

  sealed trait Instruction {
    def apply(ps : ProgramState) : ProgramState
  }
  case object Instruction {
    def parse(s : String) : Instruction =
      s.split(" ").toList match {
        case "nop" :: _ :: Nil =>
          NoOperation
        case "acc" :: n :: Nil =>
          Accumulate(n.toInt)
        case "jmp" :: n :: Nil =>
          Jump(n.toInt)
        case s =>
          throw new IllegalArgumentException(s"Invalid instruction: $s")
      }
  }
  case object NoOperation extends Instruction {
    def apply(ps : ProgramState) : ProgramState =
      ps.copy(instructionPointer = ps.instructionPointer + 1)
  }
  case class Accumulate(n : Int) extends Instruction {
    def apply(ps : ProgramState) : ProgramState =
      ps.copy(
        instructionPointer = ps.instructionPointer + 1,
        accumulator = ps.accumulator + n
      )
  }
  case class Jump(n : Int) extends Instruction {
    def apply(ps : ProgramState) : ProgramState =
      ps.copy(
        instructionPointer = ps.instructionPointer + n
      )
  }

  case class ProgramState(instructionPointer: Int, accumulator: Int, instructionsDone : Set[Int]) {
    @tailrec
    final def run(instructions : IndexedSeq[Instruction]) : ProgramState =
      if (instructionsDone.contains(instructionPointer))
        this
      else
        instructions(instructionPointer)
          .apply(this.copy(instructionsDone = instructionsDone + instructionPointer))
          .run(instructions)
  }
  object ProgramState {
    def empty: ProgramState =
      ProgramState(0, 0, Set.empty)
  }

  val f = file"./files/day8/input"
  val instructions = f.lines.map(Instruction.parse).toIndexedSeq

  println("Day8A")
  println(ProgramState.empty.run(instructions).accumulator)

}
