package assessments

import assessments.Assessment.graderIncomplete
import assessments.Exam.{ExamMainRun, runOption}
import assessments.ExceptionContext.{addToExceptionContext, initialExceptionContext}
import assessments.GradingContext.comments
import assessments.InterpolatedMarkdown.md
import assessments.pageelements.{AnswerElement, DynamicElement, Element, ElementAction, ProblemElement, StaticElement}
import example_exam.ExampleProblem.question
import externalsystems.{Dynexite, MoodleStack}
import org.apache.commons.text.StringEscapeUtils
import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer
import play.api.libs.json.JsValue
import sourcecode.{File, Line}
import utils.Markdown.markdownToHtml
import utils.Tag.Tags
import utils.{Tag, Utils}
import utest.{TestSuite, Tests}
import utils.Utils.awaitResult

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.collection.{SeqMap, mutable}
import scala.compiletime.uninitialized
import scala.concurrent.duration.Duration
import scala.util.Try
import scala.util.matching.Regex

abstract class MarkdownAssessment extends TestSuite {
  given ExceptionContext = initialExceptionContext(s"Initializing problem ${getClass.getName}")
  val name: String = getClass.getName
  /** ID of this assessment (guaranteed unique within an [[Exam]]) */
  val id: String = getClass.getName
  lazy val question: InterpolatedMarkdown[Element | HtmlConvertible]

  // Root node; created in `initTests`
  private var testCases: Test = uninitialized
  def getTests: Test = { initDefaultTests; testCases }

  @deprecated("Use inline graders")
  def grade()(using context: GradingContext, exceptionContext: ExceptionContext): Unit = {}
  lazy val reachablePoints: Points

  private def findMethod(elementName: ElementName) =
    this.getClass.getMethod(elementName.toString.replace('.','$')).invoke(this)

  final lazy val assessment: Assessment = {
    given ExceptionContext = ExceptionContext.initialExceptionContext(s"Markdown assessment $name")
    val seen = mutable.HashSet[ElementName]()
    val elements = SeqMap.newBuilder[ElementName, DynamicElement]

    val questionTemplate = {
      val date = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
      val clazz = this.getClass.getName.stripSuffix("$")
//      val comment = InterpolatedHtml(Html(s"<!-- Exported via Dominique Unruh's assessment tool. Source class ${StringEscapeUtils.escapeHtml4(clazz)}. Date: ${StringEscapeUtils.escapeHtml4(date)} -->\n"))
      //comment ++
      question.toHtml.inlineHtmlConvertible
    }

    for (case element: DynamicElement <- questionTemplate.args) {
      if (!seen.add(element.name))
        throw ExceptionWithContext(s"Duplicate page element name '${element.name}'")
      elements.addOne(element.name, element)
    }

    Assessment(name = name,
      questionTemplate = questionTemplate,
      reachablePoints = reachablePoints,
      pageElements = elements.result(), tags = tags)
  }

  val tags: Tags[Assessment] = Tags.empty

  protected def testSolution(expected: Points = reachablePoints,
                   changes: Seq[(DynamicElement, String)] = Seq.empty): Test =
    Test(s"solution: $changes, expected: $expected") {
    println(s"Testing $name with ${if (changes.nonEmpty) "modified " else ""}reference solution, expected: $expected points.")
    val originalReference = for (case (name, answerElement: AnswerElement) <- assessment.pageElements)
      yield name -> answerElement.reference
    val changedReference = mutable.Map(originalReference.toSeq *)
    for ((pageElement, value) <- changes)
      if (pageElement == null)
        throw ExceptionWithContext(s"Changed contain a null (the changed answer element)", value, changedReference)
      else
        changedReference(pageElement.name) = value

    println(s"Reference solution: ${changedReference.map((k, v) => s"$k -> $v").mkString(", ")}")
    val points = assessment.pointsReached(changedReference.toMap, None).awaitResult()
    println(s"Resulting number of points: $points (expected points: $expected)")
    if (points != expected)
        throw ExceptionWithContext("Mismatch with expectation")
  }

  private def defaultTests(): Unit = {
    val problemElements = question.args.collect { case e : ProblemElement => e }
    if (problemElements.nonEmpty)
      addTest(Test("checking for ProblemElement's") {
            throw ExceptionWithContext(s"Encountered problems/todos: ${problemElements.mkString(", ")}")
      })

    if (!tags(graderIncomplete))
      addTest(testSolution().withName("Reference solution, full points?"))

    val emptyReference =
      for (case (answerElement: AnswerElement) <- assessment.pageElements.values)
        yield answerElement -> ""
    addTest(testSolution(changes = emptyReference.toSeq, expected = 0).withName("No answers, no points?"))

    addTest(Test("Class name") {
      def cleanup(input: String): String = {
        val words = input.replaceAll("[^\\w\\d]", " ").split("\\s+").filter(_.nonEmpty)
        words.map(_.toLowerCase.capitalize).mkString
      }
      val className = MarkdownAssessment.this.getClass.getSimpleName.stripSuffix("$")
      if (className.replaceAll("[^\\w\\d]", "").toLowerCase != name.replaceAll("[^\\w\\d]", "").toLowerCase)
        throw ExceptionWithContext(s"Name ($name) and class name ($className) don't match. Use, e.g., ${cleanup(name)} as the class name, so $className (with extra spaces) as name")
    })
  }

  private lazy val initTests: Unit = {
    assert(name != null)
    testCases = Test(name, Seq.empty) // `name` is safe to read now (lazy val ⇒ forced post-construction)
  }

  private lazy val initDefaultTests: Unit = {
    initTests
    defaultTests()
  }

  def addTest(test: Test)(using file: File, line: Line): Unit = synchronized {
    initTests
    testCases = testCases.appendChild(test.there(file, line))
  }

  override def tests: Tests = {
    given ExceptionContext = initialExceptionContext(s"Running tests for problem '$name'")
    getTests.toTests
  }

  /** Run selftests of this assessment */
  def runTests()(using exceptionContext: ExceptionContext): Unit = {
    given ExceptionContext = addToExceptionContext(s"Running tests for question $name")
    getTests.runAll()
  }

  def main(args: Array[String]): Unit = {
    Utils.loadSystemProperties()
    given ExceptionContext = initialExceptionContext(s"Running main for problem '$name'")
    println(s"Running the main method of \"$name\", with run option $runOption (configured in java.properties).")
    if (ExamMainRun.values.length > 1)
      println(s"To configure a different action, set MarkdownAssessment.runOption to one of: ${(ExamMainRun.values.toSet - runOption).mkString(", ")}")

    runOption match {
      case ExamMainRun.runTests => mainRunTests()
      case ExamMainRun.extractStack => mainExtractStack()
      case ExamMainRun.uploadDynexite => mainUploadDynexite()
    }
  }

  def mainRunTests(implicit exceptionContext: ExceptionContext): Unit = {
    runTests()
  }

  def mainExtractStack(implicit exceptionContext: ExceptionContext): Unit = {
    val testResult = Try(runTests())
    for (exception <- testResult.failed)
      exception.printStackTrace()
    val question = MoodleStack.assessmentToQuestion(assessment)
    val quiz = MoodleStack.Quiz(question)
    val pretty = quiz.prettyXml
    println(pretty)
    Utils.copyStringToClipboard(pretty)
    println("Copied to clipboard. You have 60s to paste it.")
    if (testResult.isFailure)
      println("*** WARNING: tests failed ***")
    Thread.sleep(60000)
    println("Time expired.")
  }

  def uploadToDynexite()(implicit exceptionContext: ExceptionContext): Unit = {
    val questionId = assessment.tags.getOrElse(Dynexite.dynexiteQuestionId,
      throw ExceptionWithContext(s"Problem '$name' has no tag dynexiteQuestionId; cannot upload to Dynexite."))
    println("Going to upload: " + Dynexite.editUrl(questionId))
    val expectedName = assessment.tags.getOrElse(Dynexite.dynexiteQuestionName, name)
    val title = Plaintext(name).toMarkdown.markdown
    val question = MoodleStack.assessmentToQuestion(assessment)
    val pretty = MoodleStack.Quiz(question).prettyXml
    println(s"Uploading question '$name' to Dynexite item $questionId (expecting name '$expectedName') ...")
    Dynexite.markReviewedAndUpload(questionId, pretty, expectedName, title, assessment.reachablePoints)
    println(s"Uploaded.")
    println(Dynexite.editUrl(questionId))
  }

  def mainUploadDynexite(implicit exceptionContext: ExceptionContext): Unit = {
    val testResult = Try(runTests())
    for (exception <- testResult.failed)
      exception.printStackTrace()
    uploadToDynexite()
    if (testResult.isFailure)
      println("*** WARNING: tests failed ***")
  }
}

object MarkdownAssessment {
  private val tagFindingRegex: Regex = """(?s)\{\{(.*?)}}""".r
  private val startTagRegex: Regex = """<(.*?)>""".r
  private val endTagRegex: Regex = """</(.*?)>""".r
  private val fieldNameRegex: Regex = """([a-zA-Z_][a-zA-Z0-9_]*)""".r
  private val latexTag: Regex = """latex:(?s)\s*(.*?)""".r

  given Conversion[MarkdownAssessment, Assessment] = _.assessment
}