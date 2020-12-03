import better.files._


object Day3A extends App {
  sealed trait Location{
    def isTree : Boolean
  }
  object Location {
    def parse(c : Char) : Location = c match {
      case '#' => Tree
      case '.' => Empty
    }
  }
  case object Tree extends Location {
    override def isTree: Boolean = true
  }
  case object Empty extends Location {
    override def isTree: Boolean = false
  }

  case class Row(locations : Seq[Location]) {
    def apply(i : Int) : Location =
      locations(i % locations.length)
  }
  object Row {
    def parse(s : String) : Row =
      Row(s.map(Location.parse))
  }

  case class Map(rows : Seq[Row]) {
    def apply(i : Int) : Row =
      rows(i)
  }
  object Map {
    def parse(ss : Seq[String]) : Map =
      Map(ss.map(Row.parse))
  }


  val f = file"./files/day3/input"
  val map = Map.parse(f.lines.toSeq)

  val xRange = Range(start = 0, end = Int.MaxValue, step = 3)
  val yRange = Range(start = 0, end = map.rows.length, step = 1)

  val coordinates = xRange.zip(yRange)
  println(coordinates)
  val path = coordinates.map{ case (x, y) => map(y)(x)}

  def numberOfTrees : Int = path.count(_.isTree)

  println("Day3A")
  println(s"$numberOfTrees trees")
}
