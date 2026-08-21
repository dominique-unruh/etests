package assessments

import assessments.Exam.{ExamMainRun, runOption}
import assessments.ExceptionContext.{addToExceptionContext, initialExceptionContext}
import assessments.GradingContext.{Outcome, answersImmutable, comments}
import assessments.InterpolatedMarkdown.md
import assessments.pageelements.GradingElement.Feedback
import assessments.pageelements.{AnswerElement, DynamicElement, Element, ElementAction, GradingElement, ProblemElement, StaticElement}
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
      sourceAssessment = this,
      pageElements = elements.result(), tags = tags)
  }

  val tags: Tags[Assessment] = Tags.empty

  lazy val referenceSolution : Answers =
    Answers(
      answers = (for (case (name, answerElement: AnswerElement) <- assessment.pageElements) yield name -> answerElement.reference).toMap,
      description = "reference solution")

  lazy val emptySolution : Answers =
    Answers(
      answers = (for (name <- assessment.pageElements.keys) yield name -> "").toMap,
      description = "empty solution")

  /** The values of all `val`/`lazy val`s of type [[Answers]] declared in this class (found via reflection),
   * prefixed with the inherited [[referenceSolution]] and [[emptySolution]] (which reflection over
   * `getDeclaredMethods` does not pick up, since they are declared in [[MarkdownAssessment]]). */
  lazy val testingSolutions: Seq[Answers] =
    val declared =
      for (method <- getClass.getDeclaredMethods.toSeq
           if method.getParameterCount == 0 && method.getReturnType == classOf[Answers])
        yield method.invoke(this).asInstanceOf[Answers]
    (Seq(referenceSolution, emptySolution) ++ declared).distinct

  @deprecated("Use testGrader instead to test a single grading element")
  protected def testSolution(expected: Points = reachablePoints,
                   changes: Seq[(AnswerElement, String)] = Seq.empty): Test =
    Test(s"solution: $changes, expected: $expected") {
    println(s"Testing $name with ${if (changes.nonEmpty) "modified " else ""}reference solution, expected: $expected points.")
    val changedReference = referenceSolution.update(changes)
    println(s"Reference solution: ${changedReference}")
    val points = assessment.pointsReached(changedReference, None).awaitResult()
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

  def grader(name: String): GradingElement =
    this.pageElements(ElementName(name)).asInstanceOf[GradingElement]
  def testGrader(grader: String | GradingElement,
                 solution: Answers,
                 outcome: Outcome = null,
                 points: Points = null,
                 test: Feedback => (GradingContext, ExceptionContext) ?=> Unit = null,
                 name: String = null): Unit = {
    val gradingElement = grader match {
      case name: String => this.grader(name)
      case element: GradingElement => element
    }
    val testName =
      if (name != null) name
      else s"Grader ${gradingElement.name} with ${solution.description}"
    val testCase = Test(testName) {
      given context: GradingContext = GradingContext(solution.answers, "NO STUDENT", reachablePoints, this)
      val feedback = gradingElement.computeFeedback(
        assessment = this,
        registrationNumber = None,
        answers = solution,
        catchExceptions = false).awaitResult()
      if (outcome != null)
        assert(outcome == feedback.outcome, s"Expected: $outcome, got: ${feedback.outcome}")
      if (points != null)
        assert(points == feedback.points.get)
      if (test != null)
        test(feedback)
    }
    addTest(testCase)
  }

  /** Registers a test asserting that grading `solution` with the given grader THROWS (rather than
   * producing a feedback). Graders throw to flag a case they deliberately do not handle (see
   * `doc/graders.md`), e.g. an unrecognized input token; this pins that behavior down.
   *
   * @param grader   the grading element, by name or directly.
   * @param solution the answers to grade against.
   * @param name     the test name; defaults to `"Grader <name> throws with <solution>"`.
   */
  def testGraderThrows(grader: String | GradingElement,
                       solution: Answers,
                       name: String = null): Unit = {
    val gradingElement = grader match {
      case name: String => this.grader(name)
      case element: GradingElement => element
    }
    val testName =
      if (name != null) name
      else s"Grader ${gradingElement.name} throws with ${solution.description}"
    val testCase = Test(testName) {
      given context: GradingContext = GradingContext(solution.answers, "NO STUDENT", reachablePoints, this)
      val result = Try(gradingElement.computeFeedback(
        assessment = this,
        registrationNumber = None,
        answers = solution,
        catchExceptions = false).awaitResult())
      assert(result.isFailure)
    }
    addTest(testCase)
  }

  /** Registers a test (or one test per solution) that checks the assessment as a whole, rather than a
   * single grading element.
   *
   * @param solution the answers to grade against. If `null` (default), the test is run once for **each**
   *                 [[Answers]] `val`/`lazy val` declared in this class (see [[testingSolutions]]),
   *                 producing one sub-test per solution grouped under `name`.
   * @param points   if non-null, asserts that the total points reached (summed over all grading
   *                 elements) equal this value.
   * @param test     an optional extra check, run with a [[GradingContext]] for the current solution in
   *                 scope; use it for assertions the built-in `points` check cannot express.
   * @param name     the (mandatory) test name; also the group name when running over multiple solutions.
   */
  def testOverall(solution: Answers = null,
                  points: Points = null,
                  test: (GradingContext, ExceptionContext) ?=> Unit = {},
                  name: String): Unit = {
    val solutions = if (solution == null) testingSolutions else Seq(solution)
    val testCases = for (solution <- solutions) yield Test(solution.description) {
      given context: GradingContext = GradingContext(solution.answers, "NO STUDENT", reachablePoints, this)
      if (points != null) {
        val pointsReached = this.pointsReached(answersImmutable, Some(context.registrationNumber)).awaitResult()
        assert(points == pointsReached)
      }
      test
    }
    assert(testCases.nonEmpty)
    val testCase =
      if (testCases.length == 1)
        testCases.head.withName(name)
      else
        Test(name, testCases)
    addTest(testCase)
  }

  /** Tests whether at most one of the given graders triggers.
   * More specifically: if one is correct or partially correct or partially correct full points or has nonzero points,
   *      then all later ones need to be notApplicable. And if a grader is notApplicable,
   *      at least one earlier in the chain needs to have triggered.
   *
   * The graders are considered in the given order, which must be their priority order (highest first).
   * A grader "triggers" if its outcome is `correct`, `partiallyCorrect`, or `partiallyCorrectFullPoints`,
   * or it awards nonzero points.
   *
   * @param solution the answers to grade against. If `null` (default), the check is run once for **each**
   *                 [[Answers]] `val`/`lazy val` declared in this class (see [[testOverall]] / [[testingSolutions]]).
   * @param name     the test name; defaults to `"grader chain: <grader1>, <grader2>, ..."`.
   * @param graders  the grading elements (by name or directly), in descending priority order.
   */
  def testGraderChain(solution: Answers = null,
                      name: String = null,
                      graders: Seq[String | GradingElement]): Unit = {
    def graderNames = graders.map { case e : GradingElement => e.name.toString; case e : String => e }
    val testName = Option(name).getOrElse(s"grader chain: ${graderNames.mkString(", ")}")
    def testChain(using context: GradingContext, exceptionContext: ExceptionContext): Unit = {
      val answers = answersImmutable
      val elements = graders.map {
        case e: GradingElement => e
        case n: String => grader(n)
      }
      val feedbacks = elements.map { element =>
        (element, element.computeFeedback(assessment = this, registrationNumber = None, answers = answers, catchExceptions = false).awaitResult())
      }
      def triggered(feedback: Feedback): Boolean =
        feedback.outcome == Outcome.correct ||
          feedback.outcome == Outcome.partiallyCorrect ||
          feedback.outcome == Outcome.partiallyCorrectFullPoints ||
          feedback.points.exists(_ != Points.zero)
      // If a grader triggered, every later grader must be notApplicable.
      for (i <- feedbacks.indices if triggered(feedbacks(i)._2); j <- (i + 1) until feedbacks.length)
        assert(feedbacks(j)._2.outcome == Outcome.notApplicable,
          s"Grader ${feedbacks(i)._1.name} triggered, but later grader ${feedbacks(j)._1.name} is ${feedbacks(j)._2.outcome} (expected notApplicable) for ${answers}.")
      // If a grader is notApplicable, some earlier grader must have triggered.
      for (i <- feedbacks.indices if feedbacks(i)._2.outcome == Outcome.notApplicable)
        assert(feedbacks.take(i).exists { case (_, fb) => triggered(fb) },
          s"Grader ${feedbacks(i)._1.name} is notApplicable, but no earlier grader triggered for ${answers}.")
    }
    testOverall(solution = solution, test = testChain, name = testName)
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