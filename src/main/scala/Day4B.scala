import better.files._

object Day4B extends App{
  val f = file"./files/day4/input"

  case class Passport(m : Map[String, String]) {
    def requiredFields = Set("ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt")

    def isValid : Boolean =
      requiredFields.forall(requiredField => m.keys.exists(_ == requiredField)) &&
        birthYearValid && issueYearValid && expirationYearValid && heightValid &&
        hairColorValid && eyeColorValid && passportIdValid

    def birthYearValid : Boolean =
      m.get("byr").exists(yr => yr.length == 4 && yr.toInt >= 1920 && yr.toInt <= 2002)

    def issueYearValid : Boolean =
      m.get("iyr").exists(yr => yr.length == 4 && yr.toInt >= 2010 && yr.toInt <= 2020)

    def expirationYearValid : Boolean =
      m.get("eyr").exists(yr => yr.length == 4 && yr.toInt >= 2020 && yr.toInt <= 2030)

    def heightValid : Boolean =
      heightInCmsValid || heightInInchesValid

    private def heightInCmsValid : Boolean =
      m.get("hgt").exists(hgt =>  hgt.matches("[0-9]{3}cm") && hgt.substring(0, 3).toInt >= 150 && hgt.substring(0, 2).toInt <= 193)

    private def heightInInchesValid : Boolean =
      m.get("hgt").exists(hgt =>  hgt.matches("[0-9]{2}in") && hgt.substring(0, 2).toInt >= 59 && hgt.substring(0, 2).toInt <= 76)

    def hairColorValid : Boolean =
      m.get("hcl").exists(hcl => hcl.matches("#[0-9a-z]{6}"))

    val validEyeColors = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
    def eyeColorValid : Boolean =
      m.get("ecl").exists(validEyeColors.contains)

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
