package assessments

import assessments.Exam.{ExamMainRun, runOption}
import assessments.ExceptionContext.{addToExceptionContext, initialExceptionContext}
import assessments.GradingContext.{GraderOutcome, answersImmutable, comments}
import assessments.InterpolatedMarkdown.md
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
    val points = assessment.pointsReached(DummyExam, changedReference, None).awaitResult()
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

    // On the reference solution: no partial rule should fire, every full (non-partial) positive rule
    // should fire (or be notApplicable because a higher-priority rule fired), and no negative (penalty)
    // rule should fire. `catchExceptions = false` so a throwing grader fails this test.
    addTest(Test("Reference solution: rule outcomes consistent") {
      val gradingElements = assessment.pageElements.values.collect { case e: GradingElement => e }.toSeq
      if (gradingElements.nonEmpty) {
        val feedbacks = assessment.gradeAll(DummyExam, referenceSolution, None, catchExceptions = false).awaitResult()
        for (e <- gradingElements) {
          val fb = feedbacks(e.name)
          if (!e.negative && e.partial)
            assert(!fb.fired, s"Partial rule ${e.name} fired on the reference solution (a partial rule should not fire when the answer is fully correct).")
          if (!e.negative && !e.partial)
            assert(fb.outcome != Some(GraderOutcome.doesntFire),
              s"Full rule ${e.name} did not fire on the reference solution (and is not notApplicable).")
          if (e.negative)
            assert(!fb.fired, s"Negative (penalty) rule ${e.name} fired on the reference solution (should not).")
        }
      }
    })

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
  /** Registers a test asserting that the grader named `grader`, run **in isolation** (ignoring
   * `unless`), yields the given [[GraderOutcome]] on `solution`. This tests the grader's own verdict;
   * the interplay with other rules (mutual exclusion via `unless`, resulting `notApplicable`) is
   * covered by [[testGraderGroup]] and the built-in reference-solution test.
   *
   * @param grader   the grading element, by name or directly.
   * @param solution the answers to grade against.
   * @param outcome  the expected [[GraderOutcome]] (`fires` / `firesPartially(...)` / `doesntFire`);
   *                 if `null`, the outcome is not asserted (use `test` instead).
   * @param test     an optional extra check on the rendered rule text (including any comments the
   *                 grader produced), e.g. to assert a specific comment was added.
   * @param name     the test name; defaults to `"Grader <name> with <solution>"`. */
  def testGrader(grader: String | GradingElement,
                 solution: Answers,
                 outcome: GraderOutcome = null,
                 test: Html => Unit = null,
                 name: String = null): Unit = {
    val gradingElement = grader match {
      case name: String => this.grader(name)
      case element: GradingElement => element
    }
    val testName =
      if (name != null) name
      else s"Grader ${gradingElement.name} with ${solution.description}"
    val testCase = Test(testName) {
      val (result, html) = gradingElement.runGrader(
        exam = DummyExam, assessment = this.assessment, registrationNumber = None, answers = solution).awaitResult()
      if (outcome != null)
        assert(outcome == result, s"Expected: $outcome, got: $result")
      if (test != null)
        test(html)
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
      val result = Try(gradingElement.runGrader(
        exam = DummyExam, assessment = this.assessment, registrationNumber = None, answers = solution).awaitResult())
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
        val pointsReached = this.pointsReached(DummyExam, answersImmutable, Some(context.registrationNumber)).awaitResult()
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

  /** Tests that **at most one** of the given graders fires in the *actual* grading of `solution` —
   * i.e. grading the whole assessment via [[Assessment.gradeAll]], which honors each rule's `unless`
   * (a rule that is suppressed by an `unless` shows as `notApplicable`, not fired). Use this for a
   * priority chain of mutually-exclusive rules: it verifies that the rules together with their
   * `unless` wiring never award two of the group's rules at once — the real double-credit failure
   * mode. Because `unless` is applied here, the graders need **not** be mutually exclusive in
   * isolation: a lower rule may fire on its own as long as a higher rule's `unless` suppresses it in
   * the joint grading.
   *
   * Runs with `catchExceptions = false`, so a grader that throws (e.g. `missingGrader`) fails the
   * test rather than being silently counted as not-fired.
   *
   * @param graders  the grading elements (by name or directly) that should be mutually exclusive.
   * @param solution the answers to grade against. If `null` (default), the check runs once for **each**
   *                 [[Answers]] `val`/`lazy val` declared in this class (see [[testingSolutions]]).
   * @param name     the test name; defaults to `"grader group (≤1 fires): <grader1>, ..."`. */
  def testGraderGroup(graders: Seq[String | GradingElement],
                      solution: Answers = null,
                      name: String = null): Unit = {
    val elements = graders.map {
      case e: GradingElement => e
      case n: String => grader(n)
    }
    val testName = Option(name).getOrElse(s"grader group (≤1 fires): ${elements.map(_.name).mkString(", ")}")
    def check(using context: GradingContext, exceptionContext: ExceptionContext): Unit = {
      val answers = answersImmutable
      // Grade the whole assessment jointly so each rule's `unless` is applied; then count how many of
      // the group's rules actually fired (a suppressed rule is `notApplicable`, i.e. not fired).
      val feedbacks = this.assessment.gradeAll(
        exam = DummyExam, answers = answers, registrationNumber = None, catchExceptions = false).awaitResult()
      val firedGraders = elements.filter(element => feedbacks(element.name).fired)
      assert(firedGraders.length <= 1,
        s"More than one grader fired (joint grading, honoring `unless`) for $answers: ${firedGraders.map(_.name).mkString(", ")}.")
    }
    testOverall(solution = solution, test = check, name = testName)
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