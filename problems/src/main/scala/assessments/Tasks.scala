package assessments

import assessments.Comment.Kind
import assessments.ExceptionContext.initialExceptionContext
import externalsystems.Dynexite
import org.apache.commons.lang3.exception.ExceptionUtils
import org.apache.commons.text.StringEscapeUtils
import org.apache.commons.text.StringEscapeUtils.escapeHtml4
import utils.Utils

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

  private def makeQuestionReport(student: String, question: Assessment, errors: mutable.Queue[(String, Assessment, String)]): (Points, String) = {
    given ExceptionContext = initialExceptionContext(s"Creating report for $student, question '${question.name}'")
    val output = new StringBuilder
    var points: Points = 0

    output ++= s"<h1>Problem: ${escapeHtml4(question.name)}</h1>\n"

    try {
      // TODO: Render with student input + reference
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
        registrationNumber = student)
      question.pageElements(ElementName.grader).asInstanceOf[Grader].grade()(using context)
      output ++= s"Points: ${context.points.decimalFractionString(2)} of ${question.reachablePoints}\n"
      points += context.points
      output ++= "<ul>\n"
      for (comment <- context.comments) {
        if (comment.kind == Kind.warning)
          errors += ((student, question, s"Warning: ${comment.html}"))
        val style = comment.kind match {
          case Kind.feedback => ""
          case Kind.warning => "color: red"
          case Kind.debug => "color: gray"
        }
        output ++= s"""<li style="$style">${comment.html}</li>\n"""
      }
      output ++= "</ul>\n"
    } catch {
      case e: Throwable =>
        output ++= s"""<pre style="color:red">${escapeHtml4(ExceptionUtils.getStackTrace(e))}</pre>\n"""
        errors += ((student, question, s"Exception: <pre>${escapeHtml4(e.getMessage)}</pre>"))
    }
    (points, output.result())
  }

  private def makeReport(exam: Exam, student: String, targetDir: Path, errors: mutable.Queue[(String, Assessment, String)]): Unit = {
    val studentDir = targetDir.resolve(student)
    var totalPoints = Points(0)
    Files.createDirectories(studentDir)
    val reportFile = studentDir.resolve("grading.html")
    Using.resource(new PrintWriter(reportFile.toFile)) { writer =>
      writer.println("<html>")
      writer.println("<head>")
      writer.println("""<script id="MathJax-script" async src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"></script>""")
      writer.println("</head>")
      writer.println("<body>")
      writer.println("<h1>Summary</h1>")
      writer.println("<ul>")
      writer.println(s"<li>Registration number: ${escapeHtml4(student)}</li>\n")
      writer.println("</ul>")
      if (Dynexite.resultsByLearner(exam)(student).isEmpty)
        writer.write("Student did not participate")
      else {
        val pdf = Dynexite.getAnswerPDF(exam = exam, registrationNumber = student)
        Files.write(studentDir.resolve("dynexite.pdf"), pdf)
        for (question <- exam.problems) {
          val (points, report) = makeQuestionReport(student, question, errors)
          totalPoints += points
          writer.write(report)
        }
      }
      writer.write(s"<h1>Total points: ${totalPoints.decimalFractionString(2)}</h1>")
      writer.write("</body></html>")
    }
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
          writer.println(s"""<li><a href="$student/grading.html">$student, ${question.name}</a>:""")
          //        writer.println(s"""(<a href="http://localhost:9000/preview/${question.getClass.getName}/">Webapp</a>):""")
          writer.println(s"""<span style="color:red">$message</span></li>""")
        }
        writer.println("</ul>")
      }
    }
  }
  
  private def makeReports(): Unit = {
    val targetDir = Utils.getSystemPropertyPath("private.report.dir", "the directory where to write the private reports")
    val errors = mutable.Queue[(String, Assessment, String)]()
    for (student <- Dynexite.resultsByLearner(exam).keys)
      makeReport(exam, student, targetDir, errors)
    makeErrorReport(errors, targetDir.resolve("errors.html"))
    println(s"\n\nReports in $targetDir, errors in ${targetDir.resolve("errors.html")}")
  }
}