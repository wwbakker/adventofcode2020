import better.files._

object Day4A extends App{
  val f = file"./files/day4/input"

  case class Passport(m : Map[String, String]) {
    def requiredFields = Set("ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt")

    def isValid : Boolean =
      requiredFields.forall(requiredField => m.keys.exists(_ == requiredField))

    def passportIdValid : Boolean =
      m.get("pid").exists(pid => pid.matches("[0-9]{9}"))

  }
  object Passport {

    def parse(s : String) : Passport =
      Passport(s.split(System.lineSeparator() + "| ").map(_.split(":", 2).toList match {
        case fieldId :: fieldValue :: Nil =>
          (fieldId, fieldValue)
      }).toMap)
  }
  val passports =
    f.contentAsString
      .split(System.lineSeparator() + System.lineSeparator())
      .map(Passport.parse)

  passports.map(p => s"$p ${if(p.isValid) "VALID" else "INVALID"}").foreach(println)
  println(passports.count(_.isValid) + " total valid passwords")

}
