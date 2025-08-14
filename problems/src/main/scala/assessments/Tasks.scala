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
  val exam = Iqc1Exam  // TODO don't hardcode
  val targetDir = Path.of(System.getProperty("private.report.dir"))
  for (student <- Dynexite.resultsByLearner.keys) {
    val studentDir = targetDir.resolve(student)
    Files.createDirectories(studentDir)
    val reportFile = studentDir.resolve("grading.html")
    Using(new PrintWriter(reportFile.toFile)) { writer =>
      writer.write("<html><body>\n")
      writer.write(s"<h1>Student: ${escapeHtml4(student)}</h1>\n")
      if (Dynexite.resultsByLearner(student).isEmpty)
        writer.write("Student did not participate")
      else {
        val pdf = Dynexite.getAnswerPDF(registrationNumber = student)
        Files.write(studentDir.resolve("dynexite.pdf"), pdf)
        for (question <- exam.problems) {
          given ExceptionContext = initialExceptionContext(s"Creating report for $student, question '${question.name}'")
          writer.write(s"<h2>Question: ${escapeHtml4(question.name)}</h2>\n")
          try {
            val gradingContext = GradingContext(
              answers = Dynexite.getDynexiteAnswers(question, exam, student),
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
            case e : Throwable =>
              writer.write(s"""<pre style="color:red">${escapeHtml4(ExceptionUtils.getStackTrace(e))}</pre>""")
          }
          writer.write("\n------------------\n\n")
        }
      }
      writer.write("</body></html>")
    }
  }
}
