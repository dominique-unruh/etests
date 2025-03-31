package externalsystems

import com.github.tototoshi.csv.{CSVFormat, CSVReader, CSVWriter, DefaultCSVFormat, QUOTE_ALL, Quoting}
import externalsystems.RWTHOnlineGrades.Entry
import utils.Utils

import java.nio.file.Path
import scala.collection.mutable.ArrayBuffer
import RWTHOnlineGrades.given

class RWTHOnlineGrades private (private val headers: List[String], private val content: Vector[ArrayBuffer[String]]) {
  private lazy val headerMap: Map[String, Int] = Utils.uniqueMap(headers.zipWithIndex*)
  private lazy val registrationNumberIndex =
    Utils.uniqueMap(content.zipWithIndex.map((_,index) => new Entry(this, index).registrationNumber -> index)*)
  def byRegistrationNumber(registrationNumber: String): Entry =
    new Entry(this, registrationNumberIndex(registrationNumber))
  def apply(registrationNumber: String): Entry = byRegistrationNumber(registrationNumber)
  def save(path: Path): Unit = {
    val writer = CSVWriter.open(path.toFile, "utf8")
    writer.writeRow(headers)
    writer.writeAll(content.map(_.toSeq))
    writer.close()
  }
}

object RWTHOnlineGrades {
  /** Points to an entry in [[RWTHOnlineGrades]]. */
  class Entry private[RWTHOnlineGrades] (parent: RWTHOnlineGrades, index: Int) {
    private def getField(fieldName: String) = parent.content(index)(parent.headerMap(fieldName))
    private def setField(fieldName: String, value: String): Unit = parent.content(index)(parent.headerMap(fieldName)) = value
    /** No setter allowed for this one because this would break the index [[RWTHOnlineGrades.registrationNumberIndex]] */
    def registrationNumber: String = getField("REGISTRATION_NUMBER")
    def firstName: String = getField("FIRST_NAME_OF_STUDENT")
    def familyName: String = getField("FAMILY_NAME_OF_STUDENT")
    def email: String = getField("EMAIL_ADDRESS")
    def grade: String = getField("GRADE")
    def grade_=(grade: String): Unit = setField("GRADE", grade)
    def remark: String = getField("REMARK")
    def remark_=(remark: String): Unit = setField("REMARK", remark)
  }

  given rwthOnlineCSVFormat: CSVFormat = new DefaultCSVFormat {
    override val delimiter: Char = ';'
    override val lineTerminator: String = "\n"
    override val quoting: Quoting = QUOTE_ALL
  }

  def load(path: Path): RWTHOnlineGrades = {
    val reader = CSVReader.open(path.toFile, "utf8")
    val headers = reader.readNext().get
    val content = reader.toStream.map(_.to(ArrayBuffer)).toVector
    reader.close()
    new RWTHOnlineGrades(headers, content)
  }
}