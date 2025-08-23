package externalsystems

import com.github.tototoshi.csv.{CSVFormat, CSVReader, CSVWriter, DefaultCSVFormat, QUOTE_ALL, Quoting}
import RWTHOnlineGrades.{Entry, registrationNumberIndex, given}
import externalsystems.Spreadsheet.Row
import externalsystems.Spreadsheet.ValidationRule.UniqueColumn
import utils.Utils

import java.nio.file.Path
import scala.annotation.targetName
import scala.collection.mutable.ArrayBuffer

class RWTHOnlineGrades private (private val spreadsheet: Spreadsheet) {
  def byRegistrationNumber(registrationNumber: String): Entry = spreadsheet.lookup(registrationNumberIndex, registrationNumber)
  def apply(registrationNumber: String): Entry = byRegistrationNumber(registrationNumber)
  def save(path: Path): Unit = spreadsheet.save(path)
  def map(f: Entry => Entry): RWTHOnlineGrades =
    RWTHOnlineGrades(spreadsheet.mapRows(row => f(Entry(row)).row))
  @targetName("mapPartialFunction")
  def map(f: PartialFunction[Entry, Entry]): RWTHOnlineGrades =
    map(e => f.lift(e).getOrElse(e))
}

object RWTHOnlineGrades {
  object Headers {
    val registrationNumber = "REGISTRATION_NUMBER"
    val firstName = "FIRST_NAME_OF_STUDENT"
    val familyName = "FAMILY_NAME_OF_STUDENT"
    val remark = "REMARK"
    val email = "EMAIL_ADDRESS"
    val grade = "GRADE"
  }
  private lazy val registrationNumberIndex: Spreadsheet.Index[Entry] = Spreadsheet.Index("registration number", Headers.registrationNumber, (_, row) => Entry(row))

  class Entry private[RWTHOnlineGrades] (private [RWTHOnlineGrades] val row: Spreadsheet.Row) {
    override def toString: String = s"$firstName $familyName ($registrationNumber)"
    def registrationNumber: String = row(Headers.registrationNumber)
    def firstName: String = row(Headers.firstName)
    def familyName: String = row(Headers.familyName)
    def email: String = row(Headers.email)
    def grade: String = row(Headers.grade)
    def remark: String = row(Headers.remark)
    private def setCell(header: String, value: String) = Entry(Row(row.cells.updated(header, value)))
    def setGrade(grade: String): Entry = setCell(Headers.grade, grade)
    def setRemark(remark: String): Entry = setCell(Headers.remark, remark)
    def setFamilyName(familyName: String): Entry = setCell(Headers.familyName, familyName)
  }

  val rwthOnlineCSVFormat: Spreadsheet.Format.CSV = Spreadsheet.Format.CSV(
    new DefaultCSVFormat {
      override val delimiter: Char = ';'
      override val lineTerminator: String = "\n"
      override val quoting: Quoting = QUOTE_ALL
    },
    "utf8")

  def load(path: Path): RWTHOnlineGrades = {
    val spreadsheet = Spreadsheet.load(path, rwthOnlineCSVFormat)
      .addValidationRule(UniqueColumn(Headers.registrationNumber))
      .forgetPath // Make sure we don't overwrite original accidentally
    RWTHOnlineGrades(spreadsheet)
  }
}