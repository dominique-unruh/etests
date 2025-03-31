package externalsystems

import com.github.tototoshi.csv.{CSVFormat, CSVReader, DefaultCSVFormat}
import externalsystems.RWTHOnlineGrades.Entry
import utils.Utils

import java.nio.file.Path

class RWTHOnlineGrades private (private val headers: Map[String, Int], private val content: Vector[Vector[String]]) {
  private lazy val registrationNumberIndex =
    Utils.uniqueMap(content.zipWithIndex.map((_,index) => new Entry(this, index).registrationNumber -> index)*)
  def byRegistrationNumber(registrationNumber: String): Entry =
    new Entry(this, registrationNumberIndex(registrationNumber))
  def apply(registrationNumber: String): Entry = byRegistrationNumber(registrationNumber)
}

object RWTHOnlineGrades {
  class Entry private[RWTHOnlineGrades] (parent: RWTHOnlineGrades, index: Int) {
    private def getField(fieldName: String) = parent.content(index)(parent.headers(fieldName))
    def registrationNumber: String = getField("REGISTRATION_NUMBER")
    def firstName: String = getField("FIRST_NAME_OF_STUDENT")
    def familyName: String = getField("FAMILY_NAME_OF_STUDENT")
    def email: String = getField("EMAIL_ADDRESS")
  }

  given rwthOnlineCSVFormat: CSVFormat = new DefaultCSVFormat {
    override val delimiter: Char = ';'
    override val lineTerminator: String = "\n"
  }

  def load(path: Path): RWTHOnlineGrades = {
    val reader = CSVReader.open(path.toFile, "utf8")
    val headers = Map(reader.readNext().get.zipWithIndex *)
    val content = reader.toStream.map(_.toVector).toVector
    new RWTHOnlineGrades(headers, content)
  }
}