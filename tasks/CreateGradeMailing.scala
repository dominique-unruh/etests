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
 *    - Exam result files (PDF/HTMLs for students) copied to sciebo folder (using [[TransferGradingFiles]])
 * - Spreadsheet with regsutration number & name & email [[allStudents]]
 *
 * Text template for email at the bottom of this file (for Thunderbird MailMerge)
 * */
object CreateGradeMailing extends Task {
  val examResults = Spreadsheet.load(gradingResultSpreadsheet, CSV.default)
  val allStudentsSheet = Spreadsheet.load(allStudents, CSV.default)
  val rows = for (row <- examResults.rows) yield {
    assert(row.grade.nonEmpty)
    val grade = f"${row.grade.toDouble}%.1f"
    println(grade)
    val regno = row.student
    val scieboLink = Sciebo.getPublicReadLink(scieboRoot, scieboReportDir.resolve(regno))
    val name = allStudentsSheet.lookup("registration", regno).name
    val email = allStudentsSheet.lookup("registration", regno).email
    SeqMap(
      "name" -> name,
      "email" -> email,
      "grade" -> grade,
      "sciebo" -> scieboLink,
      "registration" -> regno)
  }
  val sheet = Spreadsheet.fromMapIterable(rows)
  sheet.save(TaskContext.lecturePrivateDir resolve "exam1/grade-mailing.csv", format = CSV.default)
  println("Done.")
}

/*

To: {{name}} {{email}}
Subject: Grade for EXAM-NAME

Dear {{name}} ({{regno}}),

in the EXAM-NAME, you received the grade: {{grade}}

You can find the detailed grading in {{sciebo}} in the file grading.pdf (or grading.html).

Note: the grade can still improve if during the online or offline exam inspection, additional points will be given.
This can even happen without you attending the exam inspection. (If other students raise a concern.)
In this case, the files in the folder above will be updated.
Therefore, before participating writing an email or coming to the offline exam inspection, make sure to re-check the newest version of the grading.pdf.

Information about the exam inspection will be sent via Moodle.

Best wishes,
Dominique.

*/
