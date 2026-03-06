import assessments.Task
import Data.*
import externalsystems.Spreadsheet.Index
import externalsystems.{RWTHOnlineGrades, Sciebo, Spreadsheet}
import utils.Utils

import java.nio.file.Path
import scala.language.postfixOps

//noinspection TypeAnnotation
object Data {
  val rwthOnlineExportFile = "/home/unruh/r/lecture-postquantum-crypto/private/exam1/registrations-2026-03-02-final.csv"
//  val rwthOnlineExportFile = "/home/unruh/cloud/qis/lectures/2025-intro-qc/exam2/rwth-upload.csv" // After exam inspection
  lazy val rwthOnlineExport =
    RWTHOnlineGrades.load(rwthOnlineExportFile)
      .assertValid()
  lazy val rwthOnlineStudents = rwthOnlineExport.students

  lazy val qualifications = Spreadsheet.load("/home/unruh/cloud/qis/lectures/2025-intro-qc/exam-students.ods",
    format=Spreadsheet.Format.ODS(sheetName = "HW grades"))
  lazy val specialCases = Spreadsheet.load("/home/unruh/cloud/qis/lectures/2025-intro-qc/exam-students.ods",
    format=Spreadsheet.Format.ODS(sheetName = "Special cases"))

  lazy val qualifiedStudents = {
    for (row <- qualifications.rows.view;
         qualified = row("Qualified");
         _ = assert(qualified == "" || qualified == "1");
         if qualified == "1") yield
      row("Registration number")
  } toSeq

  /** Students participating on Schein. Regno/name pairs */
  lazy val scheinStudents =
    (for (row <- specialCases.rows;
          if row("Exam") == "2";
          if row("Schein").trim.nonEmpty)
    yield row("Regno") -> row("Name")).toMap

  lazy val allStudents = rwthOnlineStudents ++ scheinStudents.keys

  val nonQualified = allStudents.toSet -- qualifiedStudents

  val participatingStudents = allStudents.toSet intersect qualifiedStudents.toSet
}

given Conversion[String, Path] = Path.of(_)


object GetValidRegisteredStudents extends Task {
  assert(Utils.isDistinct(qualifiedStudents))
  assert(Utils.isDistinct(rwthOnlineStudents))


  println(s"Schein students: $scheinStudents")


  assert(Utils.isDistinct(allStudents))


  for (student <- nonQualified) {
    val entry = rwthOnlineExport.byRegistrationNumber(student)
    println(s"${entry.firstName} ${entry.familyName} <${entry.email}>    $student")
  }


  assert(participatingStudents.contains("475156"))
  assert(participatingStudents.contains("475053"))
  
  println((participatingStudents.size, allStudents.size, nonQualified.size))

  Spreadsheet.fromIterable(Seq("Registration number"), participatingStudents.map(Seq(_)))
    .save("/home/unruh/cloud/qis/lectures/2025-intro-qc/exam2/participating-students-for-dynexite.csv",
      Spreadsheet.Format.CSV.default)
  
}

object GradesToRWTHOnline extends Task {
  // Update first:
  // lcd intro-qc-priv && cp -a exam2/reports/ ~/cloud/sciebo/shared/intro-qc-exam2-grading/ && cd ~/cloud/sciebo/shared/intro-qc-exam2-grading/ && git gui


  val gradeSheet = Spreadsheet.load("/home/unruh/cloud/sciebo/shared/intro-qc-exam2-grading/reports/results.csv", format=Spreadsheet.Format.CSV.default)

  val gradeSheetStudents = gradeSheet.rows.map(_("student"))
  assert(Utils.isDistinct(gradeSheetStudents))

  assert(gradeSheetStudents.toSet `subsetOf` participatingStudents)

  val gradeIndex = Index("grade", "student", (i,r) => r("grade"))

  /** Grades to publish: regno => grade/link */
  val toPublish = Map((for (row <- gradeSheet.rows) yield {
    val student = row("student")
    assert(raw"[0-9]+".r.matches(student))
    val grade = row("grade")
    val link = Sciebo.getPublicReadLink(s"/shared/intro-qc-exam2-grading/reports/$student")
    student -> (grade, link)
  })*)

  val rwthOnlineImport = rwthOnlineExport
    .map { entry =>
      toPublish.get(entry.registrationNumber) match
        case Some((grade, link)) => entry.setGrade(grade).setRemark(s"Details (available temporarily): $link")
        case None => entry.setGrade("X")
    }

  assert(rwthOnlineImport.areAllGraded)

//  val rwthUpload = "/home/unruh/cloud/qis/lectures/2025-intro-qc/exam2/rwth-upload.csv"
  val rwthUpload = "/home/unruh/cloud/qis/lectures/2025-intro-qc/exam2/rwth-upload-after-inspection.csv" // After exam inspection
  rwthOnlineImport.save(rwthUpload)

  println("\n\n\n")
  
  println(s"Upload: $rwthUpload\n")
  
  println("Schein:")
  for ((regno, name) <- scheinStudents)
    if (toPublish.contains(regno))
      println(s"$name ($regno), ${toPublish(regno)._1}, ${toPublish(regno)._2}")
    else
      println(s"$name ($regno): not participated")
      
  println("Done.")
  sys.exit()
}