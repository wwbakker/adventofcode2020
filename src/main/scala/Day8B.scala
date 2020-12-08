import better.files._

import scala.annotation.tailrec

object Day8B extends App {

  sealed trait Instruction {
    def apply(ps: ProgramState): ProgramState

    def swapJumpAndNoOperation: Instruction
  }

  case object Instruction {
    def parse(s: String): Instruction =
      s.split(" ").toList match {
        case "nop" :: n :: Nil =>
          NoOperation(n.toInt)
        case "acc" :: n :: Nil =>
          Accumulate(n.toInt)
        case "jmp" :: n :: Nil =>
          Jump(n.toInt)
        case s =>
          throw new IllegalArgumentException(s"Invalid instruction: $s")
      }
  }

  case class NoOperation(n: Int) extends Instruction {
    def apply(ps: ProgramState): ProgramState =
      ps.copy(instructionPointer = ps.instructionPointer + 1)

    override def swapJumpAndNoOperation: Instruction = Jump(n)
  }

  case class Accumulate(n: Int) extends Instruction {
    def apply(ps: ProgramState): ProgramState =
      ps.copy(
        instructionPointer = ps.instructionPointer + 1,
        accumulator = ps.accumulator + n
      )

    override def swapJumpAndNoOperation: Instruction = this
  }

  case class Jump(n: Int) extends Instruction {
    def apply(ps: ProgramState): ProgramState =
      ps.copy(
        instructionPointer = ps.instructionPointer + n
      )

    override def swapJumpAndNoOperation: Instruction = NoOperation(n)
  }

  sealed trait InstructionOutcome {
    def accumulator: Int

    def isTerminatedNormally: Boolean
  }

  case class InfiniteLoop(accumulator: Int) extends InstructionOutcome {
    override def isTerminatedNormally: Boolean = false
  }

  case class TerminatedNormally(accumulator: Int) extends InstructionOutcome {
    override def isTerminatedNormally: Boolean = true
  }

  case class ProgramState(instructionPointer: Int, accumulator: Int, instructionsDone: Set[Int]) {
    @tailrec
    final def run(instructions: IndexedSeq[Instruction]): InstructionOutcome =
      if (instructionsDone.contains(instructionPointer))
        InfiniteLoop(accumulator)
      else if (instructionPointer >= instructions.length)
        TerminatedNormally(accumulator)
      else
        instructions(instructionPointer)
          .apply(this.copy(instructionsDone = instructionsDone + instructionPointer))
          .run(instructions)
  }

  object ProgramState {
    val empty: ProgramState =
      ProgramState(0, 0, Set.empty)
  }


  val f = file"./files/day8/input"
  val instructions = f.lines.map(Instruction.parse).toIndexedSeq

  val result = instructions.indices
    .map(i => instructions.updated(i, instructions(i).swapJumpAndNoOperation))
    .map(ProgramState.empty.run)
    .find(_.isTerminatedNormally)
    .map(_.accumulator)

  println("Day8B")
  println(result)

}
