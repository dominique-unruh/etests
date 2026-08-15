package assessments

import assessments.Exam.{ExamMainRun, logger, runOption}
import assessments.ExceptionContext.{addToExceptionContext, initialExceptionContext}
import assessments.pageelements.RenderContext
import com.typesafe.scalalogging.Logger
import externalsystems.Dynexite
import io.github.classgraph.ClassGraph
import org.apache.commons.text.StringEscapeUtils.escapeHtml4
import utest.{TestSuite, Tests, test}
import utils.{IndentedInterpolator, Tag, UsingWrapper, Utils}
import utils.Tag.Tags
import utils.Utils.awaitResult

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.time.LocalDate
import scala.jdk.CollectionConverters.IterableHasAsScala
import scala.util.Try

case class Exam(name: String, tags: Tags[Exam] = Tags())(val problems: MarkdownAssessment*)
               (using sourceFileImplicit: UsingWrapper[sourcecode.File, tags.type]) extends TestSuite {
  val tests2 = Tests {
    test("hello") {???}
  }
  val id: String = getClass.getName.stripSuffix("$")
  val sourceFile: Path = Path.of(sourceFileImplicit.value.value)
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
    testCases.runAll()
  }

  def mainUploadDynexite()(implicit exceptionContext: ExceptionContext): Unit = {
    val testResult = Try(runTests())
    for (exception <- testResult.failed)
      exception.printStackTrace()

    for (problem <- problems)
      problem.uploadToDynexite()

    println("Uploaded problems:")
    for (problem <- problems)
      println(s"- ${problem.name}")

    (tags.get(Dynexite.dynexiteCourseId), tags.get(Dynexite.dynexiteExamId)) match {
      case (Some(course), Some(exam)) => println(Dynexite.examUrl(course, exam))
      case (None, _) => println(s"Set tag dynexiteCourseId on exam ${name}")
      case (_, None) => println(s"Set tag dynexiteExamId on exam ${name}")
    }

    if (testResult.isFailure)
      println("*** WARNING: tests failed ***")
  }

  def main(args: Array[String]): Unit = {
    Utils.loadSystemProperties()
    given ExceptionContext = initialExceptionContext(s"Running main for problem '$name'")
    println(s"Running the main method of \"$name\", with run option $runOption (configured in java.properties).")
    if (ExamMainRun.values.length > 1)
      println(s"To configure a different action, set MarkdownAssessment.runOption to one of: ${(ExamMainRun.values.toSet - runOption).mkString(", ")}")

    runOption match {
      case ExamMainRun.runTests => runTests()
      case ExamMainRun.extractStack => runTests()
      case ExamMainRun.uploadDynexite => mainUploadDynexite()
    }
  }

  def renderExam(outputFile: Path): Unit = {
    // TODO Make this all configurable
    val renderContext = RenderContext(RenderContext.dynamic := false)

    def problemHTML(problem: MarkdownAssessment) =
      val body =
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

  private def testCases: Test = {
    val children = Seq.newBuilder[Test]

    for (points <- tags.get(Exam.reachablePoints))
      children += Test("checking reachable points") {
          if (points != reachablePoints)
            throw AssertionError(s"Exam has ${reachablePoints} reachable points, but you specified tag \"reachablePoints := ${points}\".")
      }

    for (scale <- tags.get(Exam.gradingScale))
      children += Test("checking grading scale") {
      scale.assertCorrect(reachable = tags(Exam.reachablePoints)) }

    children ++= problems.map(_.getTests)

    Test(s"Exam $name", children.result())
  }

  override def tests: Tests = {
    given ExceptionContext = initialExceptionContext(s"Tests for exam $name")
    testCases.toTests
  }
}

object Exam {
  private val logger = Logger[Exam]

  lazy val exams: Seq[Exam] = {
    val classgraph = new ClassGraph()
      .enableClassInfo()
      .scan()
    val results = Seq.newBuilder[Exam]

    // Only discover exams whose package is a single level (e.g. `y2025_pqc1`, `example_exam`);
    // nested packages (e.g. `assessments.something`) are ignored.
    def singleLevelPackage(classInfo: io.github.classgraph.ClassInfo): Boolean = {
      val pkg = classInfo.getPackageName
      pkg.nonEmpty && !pkg.contains(".")
    }

    for (classInfo <- classgraph.getAllStandardClasses.asScala)
       if (classInfo.getName.endsWith("$") && singleLevelPackage(classInfo) && classInfo.extendsSuperclass(classOf[Exam]))
         try {
//           println(classInfo)
           val clazz = classInfo.loadClass()
//           println(clazz)
           val moduleField = clazz.getDeclaredField("MODULE$")
//           println(moduleField)
           results += moduleField.get(null).asInstanceOf[Exam]
         } catch
           case e: NoSuchFieldException =>

    // Add a placeholder ArchivedExam for every subdir listed under `archived:` in exams/exams.yaml
    // that actually exists (has resources on the classpath).
    val presentDirs = classgraph.getAllResources.asScala.flatMap(_.getPath.split("/").headOption).toSet
    for (subdir <- archivedSubdirs if presentDirs.contains(subdir))
      results += new ArchivedExam(subdir)

    val exams = results.result()

    // Complain if two exams have same ID
    Utils.findCollision(exams, _.id, (x, y) =>
      throw AssertionError(s"Exam classes ${x.getClass.getName} and ${y.getClass.getName} have same id \"${x.id}\""))

    exams
  }

  /** Subdirectories of `exams/` whose entry under `exams:` in `exams/exams.yaml` has
   * `archived: true` (read from the classpath). Empty if the file is missing or unparseable. */
  private def archivedSubdirs: Seq[String] =
    try {
      val stream = getClass.getResourceAsStream("/exams.yaml")
      if (stream == null) Nil
      else {
        val content = new String(stream.readAllBytes(), UTF_8)
        io.circe.yaml.parser.parse(content).toOption
          .flatMap(_.hcursor.get[Map[String, io.circe.Json]]("exams").toOption)
          .getOrElse(Map.empty)
          .collect { case (subdir, json) if json.hcursor.get[Boolean]("archived").toOption.contains(true) => subdir }
          .toSeq
      }
    } catch case _: Throwable => Nil

  def getExamById(examId: String): Exam =
    exams.find(_.id == examId) match
      case Some(exam) => exam
      case None => throw new NoSuchElementException(s"No exam with ID $examId")


  lazy val runOption: ExamMainRun = {
    val string = Utils.getSystemProperty("run.option.for.problem", s"What to do when a problem is executed in the IDE. One of ${ExamMainRun.values.mkString(", ")}")
    try
      ExamMainRun.valueOf(string)
    catch
      case _: IllegalArgumentException => throw RuntimeException(s"System property run.option.for.problem contains illegal value. Should be one of ${ExamMainRun.values.mkString(", ")}")
  }

  enum ExamMainRun {
    case extractStack
    case runTests
    case uploadDynexite
  }


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