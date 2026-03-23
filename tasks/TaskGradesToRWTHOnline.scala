import assessments.Task
import Data.*
import exam.y2025.pqc1.{Pqc1Exam, Pqc1Students}
import externalsystems.Schein.Semester.Winter
import externalsystems.Spreadsheet.Index
import externalsystems.{RWTHOnlineGrades, Schein, Sciebo, Spreadsheet}
import utils.Utils

import java.nio.file.{Files, Path}
import scala.language.postfixOps

//noinspection TypeAnnotation
object Data {
  // This file will be inplace-updated to contain the grades
  val rwthOnlineExportImportFile = "/home/unruh/cloud/qis/lectures/2025-ws-post-quantum-cryptography/exam1/rwth-online-with-grades.csv"
  lazy val rwthOnlineExport =
    RWTHOnlineGrades.load(rwthOnlineExportImportFile)
      .assertValid()
  lazy val rwthOnlineStudents = rwthOnlineExport.students

//  lazy val qualifications = Spreadsheet.load("/home/unruh/cloud/qis/lectures/2025-intro-qc/exam-students.ods",
//    format=Spreadsheet.Format.ODS(sheetName = "HW grades"))
//  lazy val specialCases = Spreadsheet.load("/home/unruh/cloud/qis/lectures/2025-intro-qc/exam-students.ods",
//    format=Spreadsheet.Format.ODS(sheetName = "Special cases"))

//  lazy val qualifiedStudents = {
//    for (row <- qualifications.rows.view;
//         qualified = row("Qualified");
//         _ = assert(qualified == "" || qualified == "1");
//         if qualified == "1") yield
//      row("Registration number")
//  } toSeq
//
//  /** Students participating on Schein. Regno/name pairs */
//  lazy val scheinStudents =
//    (for (row <- specialCases.rows;
//          if row("Exam") == "2";
//          if row("Schein").trim.nonEmpty)
//    yield row("Regno") -> row("Name")).toMap

  val scheinStudents = Pqc1Students.scheinStudents

  lazy val allStudents = rwthOnlineStudents ++ scheinStudents.keys

//  val nonQualified = allStudents.toSet -- qualifiedStudents

  val participatingStudents = allStudents.toSet // intersect qualifiedStudents.toSet

  val scheinDir = Path.of("/home/unruh/cloud/qis/lectures/2025-ws-post-quantum-cryptography/exam1/scheine/")
}

given Conversion[String, Path] = Path.of(_)


object GetValidRegisteredStudents extends Task {
//  assert(Utils.isDistinct(qualifiedStudents))
  assert(Utils.isDistinct(rwthOnlineStudents))
  println(s"Schein students: $scheinStudents")
  assert(Utils.isDistinct(allStudents))


//  for (student <- nonQualified) {
//    val entry = rwthOnlineExport.byRegistrationNumber(student)
//    println(s"${entry.firstName} ${entry.familyName} <${entry.email}>    $student")
//  }


  println((participatingStudents.size, allStudents.size))

//  Spreadsheet.fromIterable(Seq("Registration number"), participatingStudents.map(Seq(_)))
//    .save("/home/unruh/cloud/qis/lectures/2025-intro-qc/exam2/participating-students-for-dynexite.csv",
//      Spreadsheet.Format.CSV.default)
  
}

object GradesToRWTHOnline extends Task {
  // Update first:
  // lcd intro-qc-priv && cp -a exam2/reports/ ~/cloud/sciebo/shared/intro-qc-exam2-grading/ && cd ~/cloud/sciebo/shared/intro-qc-exam2-grading/ && git gui

  // This file is created by TaskGradeEveryone
  val gradeSheet = Spreadsheet.load("/home/unruh/cloud/qis/lectures/2025-ws-post-quantum-cryptography/exam1/reports/results.csv", format=Spreadsheet.Format.CSV.default)

  val gradeSheetStudents = gradeSheet.rows.map(_("student"))
  assert(Utils.isDistinct(gradeSheetStudents))

  assert(gradeSheetStudents.toSet `subsetOf` participatingStudents,
    gradeSheetStudents.toSet -- participatingStudents
  )

  val gradeIndex = Index("grade", "student", (i,r) => r("grade"))

  /** Grades to publish: regno => grade/link */
  val toPublish = Map((for (row <- gradeSheet.rows) yield {
    val student = row("student")
    assert(raw"[0-9]+".r.matches(student))
    val grade = row("grade")
    val link = Sciebo.getPublicReadLink(s"/shared/pqc-exam1-grading/reports/$student")
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
//  val rwthUpload = "/home/unruh/cloud/qis/lectures/2025-intro-qc/exam2/rwth-upload-after-inspection.csv" // After exam inspection
  rwthOnlineImport.save(rwthOnlineExportImportFile)

  println("\n\n\n")
  
  println(s"Upload: $rwthOnlineExportImportFile\n")

  val course = Schein.Course(name = "Post-Quantum Cryptography", semester = Winter, year = 2025, ects = 6)

  println("Schein:")
  for ((regno, name) <- scheinStudents)
    if (toPublish.contains(regno)) {
      val grade = toPublish(regno)._1.toDouble
      if (grade <= 4) {
        val student = Schein.Student(name = name, registrationNumber = regno, grade = grade)
        Files.write(scheinDir.resolve(s"Schein $name $regno.pdf"), Schein.pdf(course, student))
      }
      println(s"$name ($regno), ${toPublish(regno)._1}, $grade")
    } else
      println(s"$name ($regno): not participated")

  println("Done.")
  sys.exit()
}