import better.files._


object Day14B extends App {

  val f = file"./files/day14/input"

  def maskValue(bitIndices: Seq[Int]): Long =
    bitIndices.foldLeft(0L)(setBitToOne)

  def setBitToOne(originalValue : Long, bitIndex : Int) : Long =
    originalValue | Math.pow(2, bitIndex).longValue

  def setBitToZero(originalValue : Long, bitIndex : Int) : Long =
    originalValue & ~Math.pow(2, bitIndex).longValue

  def combinations(originalValue : Long, bitIndicesXs: List[Int]) : Seq[Long] = bitIndicesXs match {
    case Nil => originalValue :: Nil
    case x :: xs =>
      combinations(setBitToOne(originalValue, x), xs) ++
      combinations(setBitToZero(originalValue, x), xs)
  }


  case class Address(v : Long)
  case class Value(v: BigInt)

  case class Mask(bitIndicesOnes: Seq[Int], bitIndicesXs: Seq[Int]) {
    def apply(v: Address): Seq[Address] = {
      val addressWithOnesApplied = maskValue(bitIndicesOnes) | v.v
      val r = combinations(addressWithOnesApplied, bitIndicesXs.toList).map(Address)
      r
    }
  }

  object Mask {
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
          bitIndicesXs = indicesFromString(maskAsString.reverse, 'X')
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
    case ((mask : Mask, c : Map[Address, Value]), SetMemory(originalAddress, value)) =>
      (mask, c ++ mask(originalAddress).map(_ -> value).toMap)
  }

  println("Day14A:")
  println(memory.map(_._2.v).sum)

}
