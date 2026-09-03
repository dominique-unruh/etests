package assessments

import assessments.RegistrationNumber.validRegitrationNumberRegex

import scala.util.matching.Regex

case class RegistrationNumber(number: String) {
  assert(validRegitrationNumberRegex.matches(number))
  override def toString: String = number
}

object RegistrationNumber {
  lazy val validRegitrationNumberRegex: Regex = """[1-9][0-9]{5}""".r.anchored
}
