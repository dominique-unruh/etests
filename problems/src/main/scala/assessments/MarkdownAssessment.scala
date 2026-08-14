package assessments

import assessments.Assessment.graderIncomplete
import assessments.Exam.{ExamMainRun, runOption}
import assessments.ExceptionContext.{addToExceptionContext, initialExceptionContext}
import assessments.GradingContext.comments
import assessments.InterpolatedMarkdown.md
import assessments.pageelements.{AnswerElement, DynamicElement, Element, ElementAction, ProblemElement, StaticElement}
import externalsystems.{Dynexite, MoodleStack}
import org.apache.commons.text.StringEscapeUtils
import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer
import play.api.libs.json.JsValue
import utils.Markdown.markdownToHtml
import utils.Tag.Tags
import utils.{Tag, Utils}
import utest.{TestSuite, Tests}

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.collection.{SeqMap, mutable}
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
  private var testCases: Test = _
  def getTests: Test = { initDefaultTests; testCases }

  @deprecated("Use inline graders")
  def grade()(using context: GradingContext, exceptionContext: ExceptionContext): Unit = {}
  lazy val reachablePoints: Points

  // TODO get rid of this
  val legacyGrader: LegacyGrader = new LegacyGrader(ElementName.grader) {
    override def grade()(using context: GradingContext, exceptionContext: ExceptionContext): Unit = {
      val duration = Utils.getSystemProperty("grading.timeout", "timeout for graders, e.g., 10s, 1m")
      Utils.runWithTimeout(Duration(duration), s"${MarkdownAssessment.this.name}-${context.registrationNumber}",
        MarkdownAssessment.this.grade())
      if (context.points > reachablePoints)
        throw ExceptionWithContext(s"Grader returned ${context.points}, but max ${reachablePoints} were reachable")
      if (context.points < 0)
        throw ExceptionWithContext(s"Grader returned ${context.points}, should be >= 0")
    }

    override lazy val reachablePoints: Points = MarkdownAssessment.this.reachablePoints
  }

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
                   changes: Seq[(DynamicElement, String)] = Seq.empty,
                   allowNoGraderYet: Boolean = true): Test = Test(s"solution: $changes, expected: $expected", {
    println(s"Testing $name with ${if (changes.nonEmpty) "modified " else ""}reference solution, expected: $expected points.")
    val originalReference = for (case (name, answerElement: AnswerElement) <- assessment.pageElements)
      yield name -> answerElement.reference
    val changedReference = mutable.Map(originalReference.toSeq *)
    for ((pageElement, value) <- changes)
      if (pageElement == null)
        throw ExceptionWithContext(s"Changed contain a null (the changed answer element)", value, changedReference)
        val name = pageElement.name
        if (!changedReference.contains(name))
          throw ExceptionWithContext(s"Unknown answer element $name", pageElement, name, value, changedReference)
        //        if (changedReference(name) == value)
        //          throw ExceptionWithContext(s"Answer element $name was updated to unchanged value $value", name, value, changedReference)
        changedReference.addOne(name -> value)

    println(s"Reference solution: ${changedReference.map((k, v) => s"$k -> $v").mkString(", ")}")
    val context = GradingContext(answers = changedReference.toMap, registrationNumber = "TEST", reachablePoints)
    try {
      legacyGrader.grade()(using context, implicitly)
      println("Resulting comments:")
      for (comment <- comments(using context))
        println("* " + comment.toPlaintext)
      println(s"Resulting number of points: ${context.points} (expected points: $expected)")
      if (context.points.get != expected)
        throw ExceptionWithContext("Mismatch with expectation")
    } catch {
      case NoGraderYetException =>
        if (allowNoGraderYet)
          println("No grader implemented yet. Not testing it.")
        else
          throw ExceptionWithContext("Grader not implemented yet.")
    }
  });

  private def defaultTests() = {
    addTest(Test("checking for ProblemElement's", {
        for (element <- question.args
             if element.isInstanceOf[ProblemElement])
          throw ExceptionWithContext(s"Encountered problem/todo: $element")
    }))

    if (!tags(graderIncomplete))
      addTest(testSolution(allowNoGraderYet = true).withName("Reference solution, full points?"))

    val emptyReference =
      for (case (answerElement: AnswerElement) <- assessment.pageElements.values)
        yield answerElement -> ""
    addTest(testSolution(allowNoGraderYet = true, changes = emptyReference.toSeq, expected = 0).withName("No answers, no points?"))

    addTest(Test("Class name", {
      def cleanup(input: String): String = {
        val words = input.replaceAll("[^\\w\\d]", " ").split("\\s+").filter(_.nonEmpty)
        words.map(_.toLowerCase.capitalize).mkString
      }
      val className = MarkdownAssessment.this.getClass.getSimpleName.stripSuffix("$")
      if (className.replaceAll("[^\\w\\d]", "").toLowerCase != name.replaceAll("[^\\w\\d]", "").toLowerCase)
        throw ExceptionWithContext(s"Name ($name) and class name ($className) don't match. Use, e.g., ${cleanup(name)} as the class name, so $className (with extra spaces) as name")
    }))
  }

  private lazy val initTests: Unit = {
    assert(name != null)
    testCases = Test(name, {}) // `name` is safe to read now (lazy val ⇒ forced post-construction)
  }

  private lazy val initDefaultTests: Unit = {
    initTests
    defaultTests()
  }

  def addTest(test: Test): Unit = synchronized {
    initTests
    testCases = testCases.appendChild(test)
  }

  override def tests: Tests = {
    given ExceptionContext = initialExceptionContext(s"Running tests for problem '$name'")
    getTests.toTests
  }

  /** Run selftests of this assessment */
  def runTests()(using exceptionContext: ExceptionContext): Unit = {
    given ExceptionContext = addToExceptionContext(s"Running tests for question $name")
    initDefaultTests
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