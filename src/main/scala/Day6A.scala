import better.files._

object Day6A extends App {
  val f = file"./files/day6/input"

  case class Group(personalAnswers : Seq[Set[Char]]) {
    def combined : Set[Char] = personalAnswers.fold(Set.empty)(_++_)
  }

  object Group {
    def parse(s : String) : Group =
      Group(s.split(System.lineSeparator()).map(_.toSet))
  }


  val result = f.contentAsString
    .split(System.lineSeparator() + System.lineSeparator())
    .map(Group.parse)
    .map(_.combined)
    .map(_.size)
    .sum


  println("Day6A")
  println(result)

}
