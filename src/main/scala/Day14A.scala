import better.files._

object Day14A extends App {

  val f = file"./files/day14/input"

  def maskValue(bitIndices: Seq[Int]): Long =
    bitIndices.foldLeft(0L) { case (v, index) => v | Math.pow(2, index).longValue }

  case class Address(v : Long)
  case class Value(v: BigInt)

  case class Mask(maskValueOnes: Long = 0, maskValueZeroes: Long = 0) {
    def apply(v: Value): Value = {
      Value((v.v | maskValueOnes) & maskValueZeroes)
    }

    override def toString: String =
      s"Mask(maskValueOnes=${maskValueOnes.toBinaryString},maskValueZeroes=${maskValueZeroes.toBinaryString})"
  }

  object Mask {
    def apply(bitIndicesOnes: Seq[Int], bitIndicesZeroes: Seq[Int]): Mask =
      Mask(maskValue(bitIndicesOnes), ~maskValue(bitIndicesZeroes))

    def empty : Mask = Mask(Nil, Nil)
  }

  trait Instruction
  case class SetMask(m: Mask) extends Instruction
  case class SetMemory(address: Address, v: Value) extends Instruction

  def indicesFromString(maskString : String, c : Char) : Seq[Int] =
    maskString.zipWithIndex.filter(_._1 == c).map(_._2)

  def parseInstruction(line: String): Instruction =
    if (line.startsWith("mask")) {
      val maskAsString = line.substring(7)
      SetMask(
        Mask(
          bitIndicesOnes = indicesFromString(maskAsString.reverse, '1'),
          bitIndicesZeroes = indicesFromString(maskAsString.reverse, '0')
        )
      )
    } else {
      val regexMatch = """mem\[([0-9]+)] = ([0-9]+)""".r.findFirstMatchIn(line).get
      SetMemory(Address(regexMatch.group(1).toLong), Value(regexMatch.group(2).toLong))
    }

  val instructions = f.lines.map(parseInstruction)
  val (_, memory) = instructions.foldLeft((Mask.empty, Map.empty[Address, Value])){
    case ((_, c : Map[Address, Value]), SetMask(mask)) =>
      (mask, c)
    case ((m : Mask, c : Map[Address, Value]), SetMemory(address, value)) =>
      (m, c.updated(address, m.apply(value)))
  }

  println("Day14A:")
  println(memory.map(_._2.v).sum)

}
