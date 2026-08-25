import TaskContext.{gradingReportDir, gradingResultSpreadsheet, scieboGradingResultSpreadsheet, scieboReportDir}
import assessments.Task
import externalsystems.Spreadsheet

import java.nio.file.{Files, Path}
import scala.sys.process.*

/** Copy the result of grading ([[gradingReportDir]]) to Sciebo folder ([[scieboReportDir]]) */
object TransferGradingFiles extends Task {
  Files.createDirectories(scieboReportDir)

  /** Parse a grading result spreadsheet (columns `student;points;grade`) into a map from student to
   * grade. */
  private def readGrades(path: Path): Map[String, Double] =
    Spreadsheet.load(path, Spreadsheet.Format.CSV.default)
      .rows.map(row => row("student") -> row("grade").toDouble).toMap

  // Abort unless grading finished without errors: errors.html must be exactly "<h1>No errors</h1>".
  val errorsFile = gradingReportDir.resolve("errors.html")
  if (!Files.exists(errorsFile))
    sys.error(s"$errorsFile does not exist; run TaskGradeEveryone first.")
  val errorsContent = Files.readString(errorsFile).trim
  if (errorsContent != "<h1>No errors</h1>")
    sys.error(s"$errorsFile does not report 'No errors'; refusing to transfer. Content: $errorsContent")

  // The results spreadsheet must exist.
  if (!Files.exists(gradingResultSpreadsheet))
    sys.error(s"$gradingResultSpreadsheet does not exist; run TaskGradeEveryone first.")

  // If grades were published to Sciebo before, make sure no already-published grade gets improved
  // (a student's grade in the old Sciebo spreadsheet must be <= their new grade), and that no
  // published student vanishes.
  if (Files.exists(scieboGradingResultSpreadsheet)) {
    val newGrades = readGrades(gradingResultSpreadsheet)
    val oldGrades = readGrades(scieboGradingResultSpreadsheet)
    for ((student, oldGrade) <- oldGrades) {
      newGrades.get(student) match {
        case None =>
          sys.error(s"Student $student is in the published Sciebo spreadsheet but not in $gradingResultSpreadsheet.")
        case Some(newGrade) if oldGrade > newGrade =>
          sys.error(s"Student $student: published grade $oldGrade is worse than new grade $newGrade (grades may not improve on re-transfer).")
        case Some(_) =>
      }
    }
  }

  // Trailing "/" on the source so rsync copies the *contents* of gradingReportDir into scieboReportDir.
  val command = Seq(
    "rsync", "-av", "--delete",
    s"$gradingReportDir/",
    scieboReportDir.toString)

  logger.info(s"Running: ${command.mkString(" ")}")
  val exitCode = command.!
  if (exitCode != 0)
    sys.error(s"rsync failed with exit code $exitCode")

  // After the copy the two spreadsheets must be byte-identical (sanity check that rsync worked).
  if (Files.mismatch(gradingResultSpreadsheet, scieboGradingResultSpreadsheet) != -1)
    sys.error(s"$scieboGradingResultSpreadsheet differs from $gradingResultSpreadsheet after rsync.")
}
