package assessments

import assessments.ExceptionContext.initialExceptionContext
import assessments.MarkdownAssessment.{MarkdownAssessmentRun, markdownToHtml}
import assessments.pageelements.{AnswerElement, Element, ElementAction, PageElement}
import externalsystems.MoodleStack
import org.apache.commons.text.StringEscapeUtils
import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer
import play.api.libs.json.JsValue
import utils.Tag.Tags
import utils.{Tag, Utils}

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.collection.{SeqMap, mutable}
import scala.util.matching.Regex

abstract class MarkdownAssessment {
  val name: String = getClass.getName
  lazy val markdown: InterpolatedString[Element]
  def grade(gradingContext: GradingContext): (Points, Seq[String])
  val reachablePoints: Points
  val grader: Grader = new Grader(ElementName("grader")) {
    override def grade(gradingContext: GradingContext): (Points, Seq[String]) = MarkdownAssessment.this.grade(gradingContext)
    override lazy val points: Points = MarkdownAssessment.this.reachablePoints
    override val tags: Tag.Tags[this.type] = Tag.Tags.empty
  }

  extension (sc: StringContext) {
    inline def md(args: Element*): InterpolatedString[Element] = InterpolatedString(sc.parts, args)
  }

  private def findMethod(elementName: ElementName) =
    this.getClass.getMethod(elementName.toString.replace('.','$')).invoke(this)

  final lazy val assessment: Assessment = {
    given ExceptionContext = ExceptionContext.initialExceptionContext(s"Markdown assessment $name")
    val seen = mutable.HashSet[ElementName]()
    val elements = SeqMap.newBuilder[ElementName, PageElement]

    val htmlTemplate = {
      val date = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
      val clazz = this.getClass.getName.stripSuffix("$")
      val comment = s"<!-- Exported via Dominique Unruh's assessment tool. Source class ${StringEscapeUtils.escapeHtml4(clazz)}. Date: ${StringEscapeUtils.escapeHtml4(date)} -->\n"
      markdown.mapCompleteString(s => comment + markdownToHtml(s))
    }

    assert(grader.name == ElementName("grader"))
    elements.addOne(grader.name, grader)
    seen.add(grader.name)

    for (case element: PageElement <- htmlTemplate.args) {
      if (!seen.add(element.name))
        throw ExceptionWithContext(s"Duplicate page element name '${element.name}'")
      elements.addOne(element.name, element)
    }

    Assessment(name = name, htmlTemplate = htmlTemplate,
      pageElements = elements.result(), tags = tags)
  }

  val tags: Tags[Assessment] = Tags.empty

  def testSolution(expected: Points = reachablePoints,
                   changes: Seq[(PageElement, String)] = Seq.empty,
                   allowNoGraderYet: Boolean = false): AssessmentTest = new AssessmentTest {
    override def runTest()(using exceptionContext: ExceptionContext): Unit = {
      println(s"Testing $name with ${if (changes.nonEmpty) "modified " else ""} reference solution.")
      val originalReference = for (case (name, answerElement: AnswerElement) <- assessment.pageElements)
        yield name -> answerElement.reference
      val changedReference = mutable.Map(originalReference.toSeq *)
      for ((pageElement, value) <- changes)
        val name = pageElement.name
        if (!changedReference.contains(name))
          throw ExceptionWithContext(s"Unknown answer element $name", pageElement, name, value, changedReference)
        if (changedReference(name) == value)
          throw ExceptionWithContext(s"Answer element $name was updated to unchanged value $value", name, value, changedReference)
        changedReference.addOne(name -> value)

      println(s"Reference solution: ${changedReference.map((k, v) => s"$k -> $v").mkString(", ")}")
      val gradingContext = GradingContext(answers = changedReference.toMap, registrationNumber = "TEST")
      try {
        val (points, comment) = grader.grade(gradingContext)
        println(s"Resulting comments:\n${comment.map(comment => "* " + comment).mkString("\n")}")
        println(s"Resulting number of points: $points (expected points: $expected)")
        assert(points == expected)
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
        throw ExceptionWithContext(s"Name ($name) and class name ($className) don't match. Use, e.g., ${cleanup(name)} as the class name, so ${className} (with extra spaces) as name")
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
    for ((name,test) <- getTests)
      println(s"""Running test $name:""")
      test.runTest()
  }

  def main(args: Array[String]): Unit = {
    given ExceptionContext = initialExceptionContext(s"Running main for $name")
    println(s"Running the main method of \"$name\", with run option $runOption.")
    if (MarkdownAssessmentRun.values.length > 1)
      println(s"To configure a different action, set MarkdownAssessment.runOption to one of: ${(MarkdownAssessmentRun.values.toSet - runOption).mkString(", ")}")

    runOption match {
      case MarkdownAssessmentRun.runTests => mainRunTests()
      case MarkdownAssessmentRun.extractStack => mainExtractStack()
    }
  }
  private val runOption = MarkdownAssessmentRun.extractStack

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
  private val markdownParser = Parser.builder.build
  private val markdownRenderer = HtmlRenderer.builder.build

  private def markdownToHtml(markdown: String): String = {
    val markdownEscaped = markdown.replace("\\", "\\\\") // commonmark parse treats \( as ( etc. So we quote the \. Could be refinded
    val parsed = markdownParser.parse(markdownEscaped)
    markdownRenderer.render(parsed)
  }

  enum MarkdownAssessmentRun {
    case extractStack
    case runTests
  }
}