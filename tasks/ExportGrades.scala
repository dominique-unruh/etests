import TaskContext.{/, allStudents, exam, gradingReportDir, gradingResultSpreadsheet, rwthOnlineCSV, scheinDir}
import assessments.Exam.courseName
import assessments.{RegistrationNumber, Task}
import externalsystems.Schein.Student
import externalsystems.Spreadsheet.Format.CSV
import externalsystems.Spreadsheet.Index
import externalsystems.{RWTHOnlineGrades, Schein, Spreadsheet}

import java.nio.file.Files

object ExportGrades extends Task {
  val results = Spreadsheet.load(gradingResultSpreadsheet, CSV.default)
  val gradeIndex = Index[String]("graded", "student", (_,r) => r("grade"))
  val rwthOnlineGrades = RWTHOnlineGrades.load(rwthOnlineCSV)
  val allStudentsSheet = Spreadsheet.load(allStudents, CSV.default)

  val gradedStudents = results.rows.map(r => RegistrationNumber(r("student"))).toSet
  val scheinStudents = gradedStudents -- rwthOnlineGrades.students

  val rwthOnlineGradesUpdated = rwthOnlineGrades.map { entry =>
    results.lookupOption("student", entry.registrationNumber.number) match {
      case None => assert(entry.grade.isEmpty); entry
      case Some(row) => entry.setGrade(row.grade)
    }
  }
  println(s"Saving grades to $rwthOnlineCSV")
  rwthOnlineGradesUpdated.save(rwthOnlineCSV)

  /** regno, name, email, filename */
  val mailing = Seq.newBuilder[Map[String, String]]
  for (regno <- scheinStudents) {
    println(s"Creating schein for $regno")
    val grade = results.lookup("student", regno.number).grade.toDouble
    if (grade > 4)
      println(s"Not needed, grade is $grade")
    else {
      val allStudentsRow = allStudentsSheet.lookup("registration", regno.number)
      val student = Student(
        name = allStudentsRow.name,
        registrationNumber = regno,
        email = Some(allStudentsRow.email),
        grade = Some(grade)
      )
      Files.createDirectories(scheinDir)
      val filename = s"Schein ${allStudentsRow.name} ${regno}.pdf"
      Files.write(scheinDir / filename, Schein.pdf(exam, student))
      mailing += Map(
        "registration" -> regno.number,
        "name" -> allStudentsRow.name,
        "email" -> allStudentsRow.email,
        "filename" -> filename)
      println(s"${allStudentsRow.name}, ${regno}, $grade")
    }
  }

  val mailingCSV = Spreadsheet.fromMapIterable(mailing.result())
  println(s"Mailing CSV -> ${scheinDir / "mailing.csv"}")
  mailingCSV.save(scheinDir / "mailing.csv", CSV.default)

  println("Done.")

}
