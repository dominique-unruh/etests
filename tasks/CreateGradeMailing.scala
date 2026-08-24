import TaskContext.{allStudents, gradingReportDir, gradingResultSpreadsheet, scieboReportDir, scieboRoot}
import assessments.Task
import externalsystems.{Sciebo, Spreadsheet}
import externalsystems.Spreadsheet.Format.CSV
import utils.Utils
import utils.Utils.sharedFile

import java.nio.file.Path
import java.sql.DriverManager
import scala.collection.SeqMap

/** Creates a CSV file for mailing all grades to the students.
 *
 * Assumes:
 * - a finished grading
 *    - Spreadsheet with registration number & grade [[gradingResultSpreadsheet]]
 *    - Exam result files (PDF/HTMLs for students) copied to sciebo folder
 * - Spreadsheet with regsutration number & name & email [[allStudents]]
 *
 * */
object CreateGradeMailing extends Task {
  val examResults = Spreadsheet.load(gradingResultSpreadsheet, CSV.default)
  val allStudentsSheet = Spreadsheet.load(allStudents, CSV.default)
  val rows = for (row <- examResults.rows) yield {
    val grade = row.grade
    val regno = row.registration
    val scieboLink = Sciebo.getPublicReadLink(scieboRoot, scieboReportDir.resolve(regno))
    val name = allStudentsSheet.lookup("registration", regno).name
    val email = allStudentsSheet.lookup("registration", regno).email
    SeqMap("name" -> name, "email" -> email, "grade" -> grade, "sciebo" -> scieboLink)
  }
  val sheet = Spreadsheet.fromMapIterable(rows)
  sheet.save(TaskContext.lecturePrivateDir resolve "exam1/grade-mailing.csv")
}
