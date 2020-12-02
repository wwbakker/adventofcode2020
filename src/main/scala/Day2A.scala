import better.files._



object Day2A extends App {
  case class Entry(min : Int, max : Int, letter : Char, password : String) {
    val letterCount : Int =
      password.count(_ == letter)

    def validPassword : Boolean =
      letterCount >= min && letterCount <= max
  }

  object Entry {
    def parse(entry : String) : Entry =
      entry.split(" ").toList match {
        case minMaxPart :: letterPart :: password :: Nil =>
          val (min, max) = parseMinMax(minMaxPart)
          Entry(min, max, parseLetter(letterPart), password)
        case _ => throw new IllegalArgumentException(s"Invalid format for entry: '$entry'")
      }

    def parseMinMax(minMax : String) : (Int, Int) =
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
