package assessments

import assessments.ExceptionContext.{addToExceptionContext, initialExceptionContext}
import assessments.GradingContext.comments
import assessments.InterpolatedMarkdown.md
import assessments.MarkdownAssessment.MarkdownAssessmentRun
import assessments.pageelements.{AnswerElement, DynamicElement, Element, ElementAction, StaticElement}
import externalsystems.MoodleStack
import org.apache.commons.text.StringEscapeUtils
import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer
import play.api.libs.json.JsValue
import utils.Markdown.markdownToHtml
import utils.Tag.Tags
import utils.{Tag, Utils}

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.collection.{SeqMap, mutable}
import scala.util.matching.Regex

abstract class MarkdownAssessment {
  val name: String = getClass.getName
  lazy val question: InterpolatedMarkdown[Element]
  lazy val explanation: InterpolatedMarkdown[Element] = md""
  lazy val gradingRules: InterpolatedMarkdown[Element] = md""
  
  def grade()(using context: GradingContext, exceptionContext: ExceptionContext): Unit
  val reachablePoints: Points
  val grader: Grader = new Grader(ElementName.grader) {
    override def grade()(using context: GradingContext, exceptionContext: ExceptionContext): Unit = {
      MarkdownAssessment.this.grade()
      if (context.points > reachablePoints)
        throw ExceptionWithContext(s"Grader returned ${context.points}, but max ${reachablePoints} were reachable")
      if (context.points < 0)
        throw ExceptionWithContext(s"Grader returned ${context.points}, should be >= 0")
    }

    override lazy val reachablePoints: Points = MarkdownAssessment.this.reachablePoints
    override val tags: Tag.Tags[this.type] = Tag.Tags.empty
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
      question.toHtml
    }

    val explanationTemplate = explanation.toHtml
    val gradingRulesTemplate = gradingRules.toHtml

    assert(grader.name == ElementName.grader)
    elements.addOne(grader.name, grader)
    seen.add(grader.name)

    for (case element: DynamicElement <- questionTemplate.args) {
      if (!seen.add(element.name))
        throw ExceptionWithContext(s"Duplicate page element name '${element.name}'")
      elements.addOne(element.name, element)
    }

    Assessment(name = name,
      questionTemplate = questionTemplate,
      explanationTemplate = explanationTemplate,
      gradingRulesTemplate = gradingRulesTemplate,
      reachablePoints = reachablePoints,
      pageElements = elements.result(), tags = tags)
  }

  val tags: Tags[Assessment] = Tags.empty

  protected def testSolution(expected: Points = reachablePoints,
                   changes: Seq[(DynamicElement, String)] = Seq.empty,
                   allowNoGraderYet: Boolean = true): AssessmentTest = new AssessmentTest {
    override def runTest()(using exceptionContext: ExceptionContext): Unit = {
      given ExceptionContext = ExceptionContext.addToExceptionContext(s"Running a test case")
      println(s"Testing $name with ${if (changes.nonEmpty) "modified " else ""}reference solution.")
      val originalReference = for (case (name, answerElement: AnswerElement) <- assessment.pageElements)
        yield name -> answerElement.reference
      val changedReference = mutable.Map(originalReference.toSeq *)
      for ((pageElement, value) <- changes)
        val name = pageElement.name
        if (!changedReference.contains(name))
          throw ExceptionWithContext(s"Unknown answer element $name", pageElement, name, value, changedReference)
//        if (changedReference(name) == value)
//          throw ExceptionWithContext(s"Answer element $name was updated to unchanged value $value", name, value, changedReference)
        changedReference.addOne(name -> value)

      println(s"Reference solution: ${changedReference.map((k, v) => s"$k -> $v").mkString(", ")}")
      val context = GradingContext(answers = changedReference.toMap, registrationNumber = "TEST", reachablePoints)
      try {
        grader.grade()(using context)
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
    }
  }

  def testName(): AssessmentTest = new AssessmentTest {
    override def runTest()(using exceptionContext: ExceptionContext): Unit = {
      def cleanup(input: String): String = {
        val words = input.replaceAll("[^\\w\\d]", " ").split("\\s+").filter(_.nonEmpty)
        words.map(_.toLowerCase.capitalize).mkString
      }
      val className = MarkdownAssessment.this.getClass.getSimpleName.stripSuffix("$")
      if (className.replaceAll("[^\\w\\d]", "").toLowerCase != name.replaceAll("[^\\w\\d]", "").toLowerCase)
        throw ExceptionWithContext(s"Name ($name) and class name ($className) don't match. Use, e.g., ${cleanup(name)} as the class name, so $className (with extra spaces) as name")
    }
  }

  def getTests: Seq[(String, AssessmentTest)] = {
    val tests = Seq.newBuilder[(String, AssessmentTest)]
//    tests += "testName" -> testName() // Automatically found below because it has no arguments
    tests += "testSolution" -> testSolution(allowNoGraderYet = true)

    for (method <- this.getClass.getMethods
         if method.getParameterCount == 0
         if classOf[AssessmentTest].isAssignableFrom(method.getReturnType))
      tests += method.getName -> method.invoke(this).asInstanceOf[AssessmentTest]

    tests.result()
  }

  /** Run selftests of this assessment */
  def runTests()(using exceptionContext: ExceptionContext): Unit = {
    given ExceptionContext = addToExceptionContext(s"Running tests for question $name")
    for ((name,test) <- getTests)
      given ExceptionContext = addToExceptionContext(s"Running test $name")
      println(s"""=================================================""")
      println(s"""Running test $name:""")
      test.runTest()
  }

  def main(args: Array[String]): Unit = {
    Utils.loadSystemProperties()
    given ExceptionContext = initialExceptionContext(s"Running main for problem '$name'")
    println(s"Running the main method of \"$name\", with run option $runOption (configured in java.properties).")
    if (MarkdownAssessmentRun.values.length > 1)
      println(s"To configure a different action, set MarkdownAssessment.runOption to one of: ${(MarkdownAssessmentRun.values.toSet - runOption).mkString(", ")}")

    runOption match {
      case MarkdownAssessmentRun.runTests => mainRunTests()
      case MarkdownAssessmentRun.extractStack => mainExtractStack()
    }
  }
  private lazy val runOption = {
    val string = Utils.getSystemProperty("run.option.for.problem", s"What to do when a problem is executed in the IDE. One of ${MarkdownAssessmentRun.values.mkString(", ")}")
    try
      MarkdownAssessmentRun.valueOf(string)
    catch
      case _ : IllegalArgumentException => throw RuntimeException(s"System property run.option.for.problem contains illegal value. Should be one of ${MarkdownAssessmentRun.values.mkString(", ")}")
  }

  def mainRunTests(implicit exceptionContext: ExceptionContext): Unit = {
    runTests()
  }

  def mainExtractStack(implicit exceptionContext: ExceptionContext): Unit = {
    runTests()
    val question = MoodleStack.assessmentToQuestion(assessment)
    val quiz = MoodleStack.Quiz(question)
    val pretty = quiz.prettyXml
    println(pretty)
    Utils.copyStringToClipboard(pretty)
    Thread.sleep(10000)
    println("Copied to clipboard. You have 10s to paste it.")
  }
}

object MarkdownAssessment {
  private val tagFindingRegex: Regex = """(?s)\{\{(.*?)}}""".r
  private val startTagRegex: Regex = """<(.*?)>""".r
  private val endTagRegex: Regex = """</(.*?)>""".r
  private val fieldNameRegex: Regex = """([a-zA-Z_][a-zA-Z0-9_]*)""".r
  private val latexTag: Regex = """latex:(?s)\s*(.*?)""".r

  enum MarkdownAssessmentRun {
    case extractStack
    case runTests
  }

  given Conversion[MarkdownAssessment, Assessment] = _.assessment
}