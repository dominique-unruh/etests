package assessments

import assessments.Comment.Kind
import assessments.ExceptionContext.initialExceptionContext
import exam.y2025.iqc1.Iqc1Exam
import externalsystems.{Dynexite, DynexiteGrader}
import org.apache.commons.io.FileUtils
import org.apache.commons.lang3.exception.ExceptionUtils
import org.apache.commons.text.StringEscapeUtils
import org.apache.commons.text.StringEscapeUtils.escapeHtml4
import utils.Utils

import java.io.{BufferedWriter, FileWriter, PrintWriter}
import java.nio.file.{Files, Path, Paths}
import scala.util.Using

abstract class Task extends App {
  Utils.loadSystemProperties()
}

//noinspection ScalaFileName
object GradeEveryone extends Task {
  val exam = Iqc1Exam // TODO don't hardcode
  makeReports()

  private def makeQuestionReport(student: String, writer: PrintWriter, question: Assessment): Unit = {
    given ExceptionContext = initialExceptionContext(s"Creating report for $student, question '${question.name}'")

    writer.write(s"<h1>Problem: ${escapeHtml4(question.name)}</h1>\n")

    try {
      // TODO: Render with student input + reference
      val answers = Dynexite.getDynexiteAnswers(question, exam, student)
      val (body, explanation) = question.renderStaticHtml(answers)

      writer.println("<h2>Question text</h2>")
      writer.println(body)

      writer.println("<h2>Solution explanation</h2>")
      writer.println(explanation)

      writer.println("<h2>Grading rules</h2>")
      // TODO
      writer.println("TODO")

      writer.println("<h2>Your grading</h2>")
      val gradingContext = GradingContext(
        answers = answers,
        registrationNumber = student)
      val commenter = Commenter()
      question.pageElements(ElementName.grader).asInstanceOf[Grader].grade(gradingContext, commenter)
      writer.write(s"Points: ${commenter.points.decimalFractionString(2)} of ${question.reachablePoints}\n")
      writer.write("<ul>")
      for (comment <- commenter.comments) {
        val style = comment.kind match
          case Kind.feedback => ""
          case Kind.warning => "color: red"
          case Kind.debug => "color: gray"
        writer.write(s"""<li style="$style">${comment.html}</li>\n""")
      }
      writer.write("</ul>")
    } catch {
      case e: Throwable =>
        writer.write(s"""<pre style="color:red">${escapeHtml4(ExceptionUtils.getStackTrace(e))}</pre>""")
    }
  }

  private def makeReport(student: String, targetDir: Path) = {
    val studentDir = targetDir.resolve(student)
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
      if (Dynexite.resultsByLearner(student).isEmpty)
        writer.write("Student did not participate")
      else {
        val pdf = Dynexite.getAnswerPDF(registrationNumber = student)
        Files.write(studentDir.resolve("dynexite.pdf"), pdf)
        for (question <- exam.problems)
          makeQuestionReport(student, writer, question)
      }
      writer.write("</body></html>")
    }
  }

  private def makeReports(): Unit = {
    val targetDir = Utils.getSystemPropertyPath("private.report.dir", "the directory where to write the private reports")
    for (student <- Dynexite.resultsByLearner.keys)
      makeReport(student, targetDir)
  }
}