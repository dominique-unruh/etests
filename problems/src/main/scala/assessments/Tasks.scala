package assessments

import assessments.ExceptionContext.initialExceptionContext
import exam.y2025.iqc1.Iqc1Exam
import externalsystems.Dynexite
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
  val targetDir = Path.of("reports")

  for (student <- Seq("***REMOVED***")) {
    val studentDir = targetDir.resolve(student)
    Files.createDirectories(studentDir)
    val reportFile = studentDir.resolve("report.txt")
    Using(new PrintWriter(reportFile.toFile)) { writer =>
      writer.write(s"Student: $student\n\n")
      if (Dynexite.resultsByLearner(student).isEmpty)
        writer.write("Student did not participate")
      else
        for (question <- exam.problems) {
          given ExceptionContext = initialExceptionContext(s"Creating report for $student, question '${question.name}'")
          writer.write(s"Question: ${question.name}\n\n")
          try {
            val gradingContext = GradingContext(
              answers = Dynexite.getDynexiteAnswers(question, exam, student),
              registrationNumber = student)
            val commenter = Commenter()
            question.pageElements(ElementName.grader).asInstanceOf[Grader].grade(gradingContext, commenter)
            writer.write(s"Points: ${commenter.points.decimalFractionString(2)} of ${question.reachablePoints}\n")
            for (comment <- commenter.comments)
              writer.write(s"* [${comment.kind}]: ${comment.html}")
          } catch {
            case e : Throwable =>
              e.printStackTrace()
              e.printStackTrace(writer)
          }
          writer.write("\n------------------\n\n")
        }
    }
  }
}
