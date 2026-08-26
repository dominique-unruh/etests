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

To: {{name}} <{{email}}>
Subject: Grade for EXAM-NAME

Dear {{name}} ({{registration}}),

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


/*

Moodle message:




Dear all,

the exam has been graded. If you participated, you should have gotten an email with the grade.
If you did not get one, please contact me.

The email also contained a link to a personal Sciebo folder with detailed grading information. The password for the folder is "password".

This Sciebo folder contains:

    grading.pdf: Your exam, with your grade and detailed information about the grading of each problem.
    grading.html: Same as HTML.
    dynexite.pdf: Your exam as downloaded directly from Dynexite. (It shows how Dynexite processed your input in case you suspect something broke into the translation into our software.)

The grade distribution (before the exam inspection, including only students who showed up) was:

TODO: Copy here.


About the exam inspection:

Exam inspection has two phases (each of them optional):

Email phase:

If you find problems in the correction, you can send them to me via email. The email must be of the following form:

* Subject must begin with POST-QUANTUM-CRYPTO EXAM INSPECTION
* The email must start with your name and registration number.
* Any comments must be preceded by name of the exam question it refers to. (Question names can be seen in the grading.pdf.)

There will be no response. There will be only an email saying "processed" so that you know it has been processed,
but not explanation how or why and whether your request was accepted.
However, the grading.pdf will be updated right away, so you should recheck there to see if something has changed.

Important: Before writing your email, you should recheck the grading.pdf before coming thought because it may be updated with corrections due to problems identified by other students.

In-person phase:

You can come to the exam inspection in the QIS chair (Monday, August 31, 10:00-11:30) whether or not you did send something by email.
You should recheck the grading.pdf before coming thought because it may be updated with corrections due to problems identified by other students.

Best wishes,
Dominique.

 */