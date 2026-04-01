import assessments.Exam.{courseName, examDate}
import assessments.pageelements.RenderContext
import assessments.{Assessment, Exam, MarkdownAssessment, Points, Task}
import com.fasterxml.jackson.annotation.JsonPropertyOrder
import io.circe.generic.auto.*
import io.circe.syntax.*
import io.circe.yaml.syntax.*
import org.apache.commons.text.StringEscapeUtils
import org.apache.commons.text.StringEscapeUtils.escapeHtml4
import utils.{IndentedInterpolator, Utils}

import java.nio.charset.StandardCharsets
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.time.LocalDate
import java.util.Date

object ArchiveExam extends Task {
  exportExam()
  
  def problemHTML(problem: MarkdownAssessment) =
    val renderContext = RenderContext(RenderContext.dynamic := false, RenderContext.studentAnswers := problem.referenceSolution)
    val (body, explanation, gradingRules) =
      problem.renderStaticHtml(renderContext)
    ind"""<h1>${escapeHtml4(problem.name)}</h1>
         |
         |<div style="">
         |  ${body.html}
         |</div>
         |
         |<div class="explanation">
         |  <h2>Explanation</h2>
         |  ${explanation.html}
         |</div>
         |
         |<div class="grading-rules">
         |  <h2>Grading rules</h2>
         |  ${gradingRules.html}
         |</div>
     """

  def exportProblem(archiveDir: Path, problem: MarkdownAssessment) = {
    val basename = problem.getClass.getSimpleName.stripSuffix("$")

    val html = ind"""<html>
                    |<head>
                    |  <title>${escapeHtml4(problem.name)}</title>
                    |  ${Assessment.htmlHeader.html}
                    |</head>
                    |<body>
                    |${problemHTML(problem)}
                    |</body>
                    |</html>
           |""".stripMargin

    val htmlFile = s"$basename.html"

    Files.write(archiveDir.resolve(htmlFile), html.getBytes(UTF_8))

    val pdfFile = s"$basename.pdf"
    Utils.htmlToPdf(archiveDir.resolve(htmlFile), archiveDir.resolve(pdfFile))

    ProblemDescription(
      name = problem.name,
      `class` = problem.getClass.getName.stripSuffix("$"),
      reachablePoints = problem.reachablePoints.toDouble,
      rendered = Seq(htmlFile, pdfFile),
    )
  }

  def exportExam() = {
    val examName = Task.Option[String]("exam to archive (full class name)", "current.exam")
    val exam = getClass.getClassLoader.loadClass(examName.value + "$").getField("MODULE$")
      .get(null).asInstanceOf[Exam]
    val basename = exam.getClass.getSimpleName.stripSuffix("$")

    val archiveDir = exam.sourceFile.getParent.resolve("archive")
    Files.createDirectories(archiveDir)

    val problems = for (problem <- exam.problems) yield
      exportProblem(archiveDir = archiveDir, problem = problem)

/*    val html = ind"""<html>
                    |<head>
                    |  <title>${escapeHtml4(exam.name)}</title>
                    |  ${Assessment.htmlHeader.html}
                    |</head>
                    |<body>
                    |${exam.problems.map(problemHTML).mkString("\n<hr/>\n")}
                    |</body>
                    |</html>
                    |""".stripMargin*/

    val examDescription = ExamDescription(
      exportedOn = LocalDate.now(),
      examDate = exam.tags(examDate),
      courseName = exam.tags(courseName),
      name = exam.name,
      reachablePoints = exam.reachablePoints,
      problems = problems)

    val printer = io.circe.yaml.Printer(preserveOrder = true)
    val yamlString = printer.pretty(examDescription.asJson)
    val yamlFile = archiveDir.resolve(s"$basename.yaml")
    Files.write(yamlFile, yamlString.getBytes(UTF_8))
  }

  @JsonPropertyOrder(
    value = Array("courseName", "name", "examDate", "reachablePoints", "exportedOn", "problems"),
    alphabetic = true)
  case class ExamDescription(
    exportedOn: LocalDate,
    examDate: LocalDate,
    courseName: String,
    name: String,
    reachablePoints: Points,
    problems: Seq[ProblemDescription])

  @JsonPropertyOrder(
    value = Array("name", "class", "reachablePoints", "rendered"),
    alphabetic = true)
  case class ProblemDescription(
    name: String,
    `class`: String,
    reachablePoints: Points,
    rendered: Seq[String])
}
