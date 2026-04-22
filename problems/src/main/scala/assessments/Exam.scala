package assessments

import assessments.Exam.logger
import assessments.ExceptionContext.{addToExceptionContext, initialExceptionContext}
import assessments.pageelements.RenderContext
import com.typesafe.scalalogging.Logger
import io.github.classgraph.ClassGraph
import org.apache.commons.text.StringEscapeUtils.escapeHtml4
import utils.{IndentedInterpolator, Tag, Utils}
import utils.Tag.Tags
import utils.Utils.awaitResult

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.time.LocalDate
import scala.jdk.CollectionConverters.IterableHasAsScala

case class Exam(name: String, tags: Tags[Exam] = Tags())(val problems: MarkdownAssessment*)
               (using sourceFileImplicit: sourcecode.File) {
  val id: String = getClass.getName.stripSuffix("$")
  val sourceFile: Path = Path.of(sourceFileImplicit.value)
  assert(problems.map(_.name).distinct.length == problems.length)
  assert(problems.map(_.id).distinct.length == problems.length)
  
  lazy val reachablePoints: Points = problems.map(_.reachablePoints).sum
  
  def assessmentIndex(assessment: Assessment)(implicit exceptionContext: ExceptionContext): Int = {
    given ExceptionContext = ExceptionContext.addToExceptionContext(s"Looking for assessment $assessment in exam", assessment, this)
    val index = problems.indexWhere(_.assessment eq assessment)
    if (index == -1)
      throw ExceptionWithContext(s"Assessment ${assessment.name} not found in exam ${this.name} (did you include the question in the exam object?)")
    index
  }

  def assessmentByName(name: String)(implicit exceptionContext: ExceptionContext): MarkdownAssessment = {
    given ExceptionContext = ExceptionContext.addToExceptionContext(s"Looking for assessment $name in exam ${this.name}", name, this)
    val assessment = problems.find(_.name == name)
    assessment.getOrElse {
      throw ExceptionWithContext(s"Assessment \"${name}\" not found in exam ${this.name}. Exact spelling matters! Available: ${problems.map(p => s"\"${p.name}\"").mkString(", ")}.")
    }
  }

  def assessmentById(id: String)(implicit exceptionContext: ExceptionContext): MarkdownAssessment = {
    given ExceptionContext = ExceptionContext.addToExceptionContext(s"Looking for assessment ID $id in exam ${this.name}", id, this)
    val assessment = problems.find(_.id == id)
    assessment.getOrElse {
      throw ExceptionWithContext(s"Assessment ID \"${id}\" not found in exam ${this.name}. Exact spelling matters! Available: ${problems.map(p => s"\"${p.id}\"").mkString(", ")}.")
    }
  }

  def runTests(): Unit = {
    given ExceptionContext = initialExceptionContext(s"Running tests for exam $name")
    for (assessment <- problems) {
      given ExceptionContext = addToExceptionContext(s"Running tests for question ${assessment.name}")
      assessment.runTests()
    }

    for (points <- tags.get(Exam.reachablePoints))
        if (points != reachablePoints)
          throw AssertionError(s"Exam has ${reachablePoints} reachable points, but you specified tag \"reachablePoints := ${points}\".")
    
    for (scale <- tags.get(Exam.gradingScale))
      scale.assertCorrect(reachable = tags(Exam.reachablePoints))
  }
  
  def main(args: Array[String]): Unit = {
    runTests()
  }

  def renderExam(outputFile: Path): Unit = {
    // TODO Make this all configurable
    val renderContext = RenderContext(RenderContext.dynamic := false)

    def problemHTML(problem: MarkdownAssessment) =
      val (body, explanation, gradingRules) =
        problem.renderStaticHtml(renderContext)
      ind"""<h2>Problem: ${escapeHtml4(problem.name)}</h1>
           |
           |<div style="">
           |  ${body.html}
           |</div>
       """

    val html =
      ind"""<html>
           |<head>
           |  <title>${escapeHtml4(name)}</title>
           |  ${Assessment.htmlHeaderStatic.html}
           |</head>
           |<body>
           |<h1>${escapeHtml4(name)}</h1>
           |${problems.map(problemHTML).mkString("\n<div class=\"problem-separator\"></div>\n")}
           |</body>
           |</html>
           |""".stripMargin


    val pdf = Utils.htmlToPdfAsync(html).awaitResult()
    Files.write(outputFile, pdf)
  }
}

object Exam {
  private val logger = Logger[Exam]

  lazy val exams: Seq[Exam] = {
    val classgraph = new ClassGraph()
      .enableClassInfo()
      .acceptPackages("exam")
      .scan()
    val results = Seq.newBuilder[Exam]

    for (classInfo <- classgraph.getAllStandardClasses.asScala)
       if (classInfo.getName.endsWith("$") && classInfo.extendsSuperclass(classOf[Exam]))
         try {
//           println(classInfo)
           val clazz = classInfo.loadClass()
//           println(clazz)
           val moduleField = clazz.getDeclaredField("MODULE$")
//           println(moduleField)
           results += moduleField.get(null).asInstanceOf[Exam]
         } catch
           case e: NoSuchFieldException =>

    val exams = results.result()

    // Complain if two exams have same ID
    Utils.findCollision(exams, _.id, (x, y) =>
      throw AssertionError(s"Exam classes ${x.getClass.getName} and ${y.getClass.getName} have same id \"${x.id}\""))

    exams
  }

  def getExamById(examId: String): Exam =
    exams.find(_.id == examId) match
      case Some(exam) => exam
      case None => throw new NoSuchElementException(s"No exam with ID $examId")

  val examDate: Tag[Exam, LocalDate] = Tag[Exam, LocalDate]()
  val courseName: Tag[Exam, String] = Tag[Exam, String]()
  /** Reachable points of the exam. If specified, running the Exam will test if this
   * matches the total of the reachable points of the problems. */
  val reachablePoints: Tag[Exam, Points] = Tag[Exam, Points]()
  val gradingScale: Tag[Exam, GradingScale] = Tag()
  /** Directory where grading reports should be written to */
  val gradingReportDir: Tag[Exam, Path] = Tag()
  /** Relative to Sciebo root */
  val scieboReportDir: Tag[Exam, Path] = Tag()
  val rwthOnlineExportImportFile: Tag[Exam, Path] = Tag()
  val scheinStudents: Tag[Exam, Map[String, String]] = Tag()
}