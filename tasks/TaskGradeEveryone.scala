import assessments.*
import assessments.Exam.{gradingReportDir, gradingScale}
import assessments.ExceptionContext.initialExceptionContext
import assessments.GradingContext.comments
import assessments.pageelements.RenderContext
import externalsystems.Dynexite
import org.apache.commons.lang3.exception.ExceptionUtils
import org.apache.commons.text.StringEscapeUtils.escapeHtml4
import utils.{DefaultValue, IndentedInterpolator, Utils}

import java.io.PrintWriter
import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.util.Using
import scala.util.control.Breaks
import scala.util.control.Breaks.{break, breakable}

//noinspection ScalaFileName
object TaskGradeEveryone extends Task {
  val includeDynexitePDFs = true
  val stopAfterFirst = false
  val generatePDFs = true
  val generateHTMLs = true
  /** For quicker processing, only a single student (or few) */
  lazy val onlyTheseStudents: Option[Seq[String]] = None

  val exam = Utils.getSystemPropertyObject[assessments.Exam]("current.exam", "the current exam")

  makeReports()

  private def makeQuestionReport(student: String, question: Assessment, errors: mutable.Queue[(String, Assessment, String)]): (Points, String) = {
    given ExceptionContext = initialExceptionContext(s"Creating report for $student, question '${question.name}'")

    val output = new StringBuilder
    var points: Points = 0

    output ++= s"<h1>Question: ${escapeHtml4(question.name)}</h1>\n"

    try {
      val answers = Dynexite.getDynexiteAnswers(question, exam, student)
      val renderContext = RenderContext(RenderContext.dynamic := false, RenderContext.studentAnswers := answers)
      val (body, explanation, gradingRules) = question.renderStaticHtml(renderContext)

      output ++= "<div class=\"question-text\">\n"
      output ++= "<h2>Question text</h2>\n"
      output ++= body.html += '\n'
      output ++= "</div>\n"

      output ++= "<div class=\"explanation\">\n"
      output ++= "<h2>Solution explanation</h2>\n"
      output ++= explanation.html += '\n'
      output ++= "</div>\n"

      output ++= "<div class=\"grading-rules\">"
      output ++= "<h2>Grading rules</h2>\n"
      output ++= gradingRules.html += '\n'
      output ++= "</div>\n"

      output ++= "<div class=\"grading-report\">"
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
    output ++= "</div>\n"
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
      val pdfLink = if (includeDynexitePDFs) """<li><a href="dynexite.pdf">Dynexite PDF</a> (for comparison)</li>""" else ""
      val title = s"${exam.name}. Student $student"
      writer.println(
        ind"""<!DOCTYPE html>
             |<html>
             |<head>
             |  <title>${escapeHtml4(title)}</title>
             |  ${Assessment.htmlHeaderStatic.html}
             |</head>
             |<body>
             |<h1>Summary</h1>
             |<ul>
             |  <li>Registration number: ${escapeHtml4(student)}</li>
             |  <li>Total points: ${totalPoints.decimalFractionString(2)} / ${exam.reachablePoints.decimalFractionString(2)}</li>
             |  <li>Grade: ${exam.tags(gradingScale).grade(totalPoints)}</li>
             |  $pdfLink
             |  <br/>${exam.tags(gradingScale).html.html}
             |</ul>
             |<hr>
             |""")

      if (includeDynexitePDFs)
        tryWithError[Unit](errors, label = "Getting Dynexite PDF", student = student) {
          val pdf = Dynexite.getAnswerPDF(exam = exam, registrationNumber = student)
          Files.write(studentDir.resolve("dynexite.pdf"), pdf)
        }
        
      for (report <- questionReports)
        writer.write(report)

      writer.write("</body></html>")
    }

    if (generatePDFs) {
      val pdfReportFile = studentDir.resolve("grading.pdf")
      Utils.htmlToPdf(reportFile, pdfReportFile)
    }

    if (!generateHTMLs) {
      Files.delete(reportFile)
    }

    totalPoints
  }

  private def makeErrorReport(errors: mutable.Queue[(String, Assessment, String)], reportPath: Path): Unit = {
    Using.resource(new PrintWriter(reportPath.toFile)) { writer =>
      if (errors.isEmpty)
        writer.println("<h1>No errors</h1>")
      else {
        def e(str: String) = escapeHtml4(str)
        writer.println(
          """<!DOCTYPE html><html><head>
            |<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/@shoelace-style/shoelace@2.20.1/cdn/themes/light.css" />
            |<script type="module" src="https://cdn.jsdelivr.net/npm/@shoelace-style/shoelace@2.20.1/cdn/shoelace-autoloader.js"></script>
            |</head>
            |<body>
            |""".stripMargin)
        writer.println(s"<h1>Errors</h1>")
        writer.println(s"There were ${errors.length} errors and warnings")
        writer.println("<ul>")
        for ((student, question, message) <- errors) {
          writer.println(s"""<li>${e(student)}<sl-copy-button value=\"${e(student)}\"></sl-copy-button>, <b>${question.name}</b>""")
          writer.println(s"""[<a target="_blank" href="${e(student)}/grading.html">Document</a> |""")
          writer.println(s"""<a target="_blank" href="http://localhost:9000/preview/${e(exam.id)}/${e(question.name)}/">Webapp</a>]<br>""")
          writer.println(s"""<span style="color:red">${message}</span></li>""")
        }
        writer.println("</ul></body>")
      }
    }
  }

  private def makePointsCSV(targetDir: Path, points: Map[String, Points], exam: Exam): Unit = {
    Using.resource(new PrintWriter(targetDir.resolve("results.csv").toFile)) { writer =>
      writer.println(s"student;points;grade")
      for ((student, points) <- points) {
        val grade = exam.tags(gradingScale).grade(points)

        writer.println(s"$student;$points;$grade")
      }
    }
  }

  private def tryWithError[A]
        (using defaultValue: DefaultValue[A])
        (errors: mutable.Queue[(String, Assessment, String)], label: String = "",
         question: MarkdownAssessment = DummyAssessment("GLOBAL"),
         fallback: A = DefaultValue.witness, student: String = "GLOBAL")
        (body: => A): A = {
    try
      body
    catch
      case e: Exception =>
//        e.printStackTrace()
        System.err.println(e.toString)
        errors += ((student, question, s"${if (label == "") "" else s"${escapeHtml4(label)}: "}<pre>${escapeHtml4(e.toString)}</pre>"))
        fallback
  }
  
  private def makeReports(): Unit = {
    val targetDir = exam.tags.getOrElse(gradingReportDir, throw RuntimeException(s"Specify gradingReportDir-tag in exam ${exam.name}"))
//    val targetDir = Utils.getSystemPropertyPath("student.report.dir", "the directory where to write the student reports")
    val errors = mutable.Queue[(String, Assessment, String)]()
    if (onlyTheseStudents.isEmpty) // Don't run time consuming things if we only want to test grade a student
      tryWithError[Unit](errors, label = "Exam tests failed") {
        exam.runTests() }

    val students = onlyTheseStudents match {
      case Some(value) => value
      case None => Dynexite.resultsByLearner(exam).toSeq.collect { case (student, results) if results.nonEmpty => student }
    }


    val pointMap = Map.newBuilder[String, Points]
    breakable {
      for (student <- students) {
        val points = makeReport(exam, student, targetDir, errors)
        pointMap += (student -> points)
        if (stopAfterFirst)
          break
      }
    }
    makeErrorReport(errors, targetDir.resolve("errors.html"))
    makePointsCSV(targetDir, pointMap.result(), exam = exam)
    println(s"\n\nReports in $targetDir, errors in ${targetDir.resolve("errors.html")}")
    if (errors.nonEmpty)
      println("***** THERE WERE ERRORS *****")
    if (onlyTheseStudents.nonEmpty)
      println(s"***** ONLY ${onlyTheseStudents.get.mkString(", ")} GRADED *****")
  }
}
