package assessments

import scala.language.implicitConversions
import assessments.Comment.Kind
import assessments.ExceptionContext.initialExceptionContext
import assessments.GradingContext.comments
import externalsystems.Spreadsheet.Index
import externalsystems.{Dynexite, RWTHOnlineGrades, Sciebo, Spreadsheet}
import org.apache.commons.lang3.exception.ExceptionUtils
import org.apache.commons.text.StringEscapeUtils
import org.apache.commons.text.StringEscapeUtils.escapeHtml4
import utils.{IndentedInterpolator, Utils}

import java.io.PrintWriter
import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.util.Using

abstract class Task extends App {
  Utils.loadSystemProperties()
}

//noinspection ScalaFileName
object GradeEveryone extends Task {
  val exam = Utils.getSystemPropertyObject[assessments.Exam]("current.exam", "the current exam")

  makeReports()

  def pointsToGrade(points: Points, reachable: Points) = {
    val percent = points / reachable * 100
    val grade: Double =
      if      percent >= 94 then 1
      else if percent >= 88 then 1.3
      else if percent >= 83 then 1.7
      else if percent >= 78 then 2
      else if percent >= 74 then 2.3
      else if percent >= 70 then 2.7
      else if percent >= 65 then 3
      else if percent >= 60 then 3.3
      else if percent >= 55 then 3.7
      else if percent >= 50 then 4
      else 5
    grade
  }

  private def makeQuestionReport(student: String, question: Assessment, errors: mutable.Queue[(String, Assessment, String)]): (Points, String) = {
    given ExceptionContext = initialExceptionContext(s"Creating report for $student, question '${question.name}'")
    val output = new StringBuilder
    var points: Points = 0

    output ++= s"<h1>Question: ${escapeHtml4(question.name)}</h1>\n"

    try {
      val answers = Dynexite.getDynexiteAnswers(question, exam, student)
      val (body, explanation, gradingRules) = question.renderStaticHtml(answers)

      output ++= "<h2>Question text</h2>\n"
      output ++= body.html += '\n'

      output ++= "<h2>Solution explanation</h2>\n"
      output ++= explanation.html += '\n'

      output ++= "<h2>Grading rules</h2>\n"
      output ++= gradingRules.html += '\n'

      output ++= "<h2>Your grading</h2>\n"
      val context = GradingContext(
        answers = answers,
        registrationNumber = student,
        question.reachablePoints)
      question.pageElements(ElementName.grader).asInstanceOf[Grader].grade()(using context)
      output ++= s"Points: ${context.points.decimalFractionString(2)} of ${question.reachablePoints}\n"
      points += context.points
      output ++= Comment.seqToHtml(Comment.filterFeedback(comments(using context).toSeq)).html
    } catch {
      case e: Throwable =>
        output ++= s"""<pre style="color:red">${escapeHtml4(ExceptionUtils.getStackTrace(e))}</pre>\n"""
        errors += ((student, question, s"Exception: <pre>${escapeHtml4(e.toString)}</pre>"))
    }
    output ++= "<hr>"
    (points, output.result())
  }

  private def makeReport(exam: Exam, student: String, targetDir: Path, errors: mutable.Queue[(String, Assessment, String)]): Points = {
    val studentDir = targetDir.resolve(student)
    var totalPoints = Points(0)
    Files.createDirectories(studentDir)
    val reportFile = studentDir.resolve("grading.html")
    val questionReports = for (question <- exam.problems) yield {
      val (points, report) = makeQuestionReport(student, question, errors)
      totalPoints += points
      report
    }

    Using.resource(new PrintWriter(reportFile.toFile)) { writer =>
      writer.println(
        ind"""<html>
          |<head>
          |    <meta charset="UTF-8">
          |    <script>
          |        window.MathJax = {
          |            tex: {
          |                inlineMath: [['$$', '$$'], ['\\\\(', '\\\\)']],
          |                displayMath: [['$$$$', '$$$$'], ['\\\\[', '\\\\]']]
          |            }
          |        };
          |    </script>
          |    <script id="MathJax-script" async src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"></script>
          |</head>
          |<body>
          |<h1>Summary</h1>
          |<ul>
          |  <li>Registration number: ${escapeHtml4(student)}</li>
          |  <li>Total points: ${totalPoints.decimalFractionString(2)} / ${exam.reachablePoints.decimalFractionString(2)}</li>
          |  <li><a href="dynexite.pdf">Dynexite PDF</a> (for comparison)</li>
          |</ul>
          |<hr>
          |""")

      val pdf = Dynexite.getAnswerPDF(exam = exam, registrationNumber = student)
      Files.write(studentDir.resolve("dynexite.pdf"), pdf)
      for (report <- questionReports)
        writer.write(report)

      writer.write("</body></html>")
    }

    totalPoints
  }

  private def makeErrorReport(errors: mutable.Queue[(String, Assessment, String)], reportPath: Path): Unit = {
    Using.resource(new PrintWriter(reportPath.toFile)) { writer =>
      if (errors.isEmpty)
        writer.println("<h1>No errors</h1>")
      else {
        writer.println(s"<h1>Errors</h1>")
        writer.println(s"There were ${errors.length} errors and warnings")
        writer.println("<ul>")
        for ((student, question, message) <- errors) {
          writer.println(s"""<li>$student, <a href="$student/grading.html">${question.name}</a>:""")
          //        writer.println(s"""(<a href="http://localhost:9000/preview/${question.getClass.getName}/">Webapp</a>):""")
          writer.println(s"""<span style="color:red">$message</span></li>""")
        }
        writer.println("</ul>")
      }
    }
  }

  private def makePointsCSV(targetDir: Path, points: Map[String, Points], reachable: Points): Unit = {
    Using.resource(new PrintWriter(targetDir.resolve("results.csv").toFile)) { writer =>
      writer.println(s"student;points;grade")
      for ((student, points) <- points) {
        val grade = pointsToGrade(points, reachable)

        writer.println(s"$student;$points;$grade")
      }
    }
  }

  private def makeReports(): Unit = {
    val targetDir = Utils.getSystemPropertyPath("student.report.dir", "the directory where to write the student reports")
    val errors = mutable.Queue[(String, Assessment, String)]()
    exam.runTests() // If this fails, let's move it into the error file or something.
    val students = Dynexite.resultsByLearner(exam).toSeq.collect { case (student, results) if results.nonEmpty => student }
    val pointMap = Map.newBuilder[String, Points]
    for (student <- students) {
      val points = makeReport(exam, student, targetDir, errors)
      pointMap += (student -> points)
    }
    makeErrorReport(errors, targetDir.resolve("errors.html"))
    makePointsCSV(targetDir, pointMap.result(), reachable = exam.reachablePoints)
    println(s"\n\nReports in $targetDir, errors in ${targetDir.resolve("errors.html")}")
    if (errors.nonEmpty)
      println("***** THERE WERE ERRORS *****")
  }
}

object GradesToRWTHOnline extends Task {
  given Conversion[String, Path] = Path.of(_)

  val rwthOnlineExport = RWTHOnlineGrades.load("/home/unruh/cloud/qis/lectures/2025-intro-qc/rwth-upload.csv")
  rwthOnlineExport.assertValid()
  val gradeSheet = Spreadsheet.load("/home/unruh/cloud/sciebo/shared/intro-qc-exam1-grading/reports/results.csv", format=Spreadsheet.Format.CSV.default)

  val rwthOnlineStudents = rwthOnlineExport.students
  val gradeSheetStudents = gradeSheet.rows.map(_("student"))
  val scheinStudents = gradeSheetStudents.toSet -- rwthOnlineStudents.toSet
  assert(Utils.isDistinct(gradeSheetStudents))
  val gradeIndex = Index("grade", "student", (i,r) => r("grade"))

  val toPublish = Map((for (row <- gradeSheet.rows) yield {
    val student = row("student")
    assert(raw"[0-9]+".r.matches(student))
    val grade = row("grade")
    val link = Sciebo.getPublicReadLink(s"/shared/intro-qc-exam1-grading/reports/$student")
    student -> (grade, link)
  })*)

  val rwthOnlineImport = rwthOnlineExport.map { entry =>
    for ((grade, link) <- toPublish.get(entry.registrationNumber)) yield
      entry.setGrade(grade).setRemark(s"Details (available temporarily): $link")
  }

  rwthOnlineImport.save("/home/unruh/cloud/qis/lectures/2025-intro-qc/rwth-upload-after-inspection.csv")

  println("Schein:\n\n")
  for (student <- scheinStudents)
    println(s"$student, ${toPublish(student)._1}, ${toPublish(student)._2}")
}