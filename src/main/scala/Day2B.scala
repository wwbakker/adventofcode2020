import better.files._


object Day2B extends App {
  case class Entry(pos1 : Int, pos2 : Int, letter : Char, password : String) {
    def validPassword : Boolean =
      password(pos1 - 1) == letter ^ password(pos2 - 1) == letter
  }

  object Entry {
    def parse(entry : String) : Entry =
      entry.split(" ").toList match {
        case positionPart :: letterPart :: password :: Nil =>
          val (min, max) = parsePositions(positionPart)
          Entry(min, max, parseLetter(letterPart), password)
        case _ => throw new IllegalArgumentException(s"Invalid format for entry: '$entry'")
      }

    def parsePositions(minMax : String) : (Int, Int) =
      minMax.split("-").toList.map(_.toInt) match {
        case min :: max :: Nil => (min, max)
        case _ => throw new IllegalArgumentException(s"Invalid format for minmax: '$minMax'")
      }

    def parseLetter(letterPart : String) : Char =
      letterPart(0)
  }

  val f = file"./files/day2/input"
  println(f.lines.map(Entry.parse).count(_.validPassword) + " valid passwords")
}
