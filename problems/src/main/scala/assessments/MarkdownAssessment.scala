package assessments

import assessments.Assessment.{tagEnd, tagStart}
import assessments.MarkdownAssessment.{markdownParser, markdownRenderer, tagFindingRegex}
import assessments.pageelements.{AnswerElement, PageElement}
import exam.y2024.pqc2.HardAssumptions
import externalsystems.LaTeX
import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer
import utils.Tag.Tags
import utils.{Tag, Utils}

import scala.collection.{SeqMap, mutable}
import scala.util.matching.Regex

abstract class MarkdownAssessment {
  val name: String = getClass.getName
  val markdown: String
  def grade(gradingContext: GradingContext): (Points, Seq[String])
  val reachablePoints: Points
  val grader: Grader = new Grader(ElementName("grader")) {
    override def grade(gradingContext: GradingContext): (Points, Seq[String]) = MarkdownAssessment.this.grade(gradingContext)
    override lazy val points: Points = MarkdownAssessment.this.reachablePoints
    override val tags: Tag.Tags[this.type] = Tag.Tags.empty
  }

  private def findMethod(elementName: ElementName) =
    this.getClass.getMethod(elementName.toString.replace('.','$')).invoke(this)

  final lazy val assessment: Assessment = {
    given ExceptionContext = ExceptionContext.initialExceptionContext(s"Markdown assessment $name")
    val seen = mutable.HashSet[ElementName]()
    val elements = SeqMap.newBuilder[ElementName, PageElement]
    val associatedFiles = Map.newBuilder[String, (String,Array[Byte])]
    var fileCounter = 0
    var elementPath = ElementPath.empty

    def addPageElement(name: String) = {
      val elementName = ElementName(elementPath, name)
      val pageElement: PageElement =
        try
          findMethod(elementName) match
            case pageElement: PageElement => pageElement
            case result =>
              throw ExceptionWithContext(s"Page element $name referenced in markdown assessment ${this.name}, but the corresponding method returns a ${result.getClass}", result)
        catch
          case _: NoSuchMethodException =>
//            for (m <- getClass.getMethods)
//              println(m)
            throw ExceptionWithContext(s"Page element $elementName referenced in markdown assessment ${this.name}, but no corresponding method found")

      if (seen.contains(elementName))
        throw new SyntaxError(s"Duplicate page element name `$name`")
      seen.add(elementName)
      elements.addOne((elementName, pageElement))
      elementName
    }

    def substitute(matsch: Regex.Match) = {
      matsch.group(1).strip() match {
        case MarkdownAssessment.endTagRegex(tag) =>
          if (!elementPath.lastOption.contains(tag))
            throw ExceptionWithContext(s"Closing tag $tag found but the path of the current group is \"$elementPath\"")
          elementPath = elementPath.removeLast
          s"<!-- Path $elementPath -->"
        case MarkdownAssessment.startTagRegex(tag) =>
          elementPath += tag
          s"<!-- Path $elementPath -->"
        case MarkdownAssessment.fieldNameRegex(name) =>
          val elementName = addPageElement(name)
          Regex.quoteReplacement(s"$tagStart$elementName$tagEnd")
        case MarkdownAssessment.latexTag(content) =>
          fileCounter += 1
          println(content)
          val png = LaTeX.tikzToPNG(content)
          val fileName = s"image$fileCounter.png"
          associatedFiles += fileName -> ("image/png", png)
          s"""<img src="$fileName"/>"""
        case _ =>
          throw ExceptionWithContext(s"Cannot parse tag: ${matsch.group(0)}")
      }
    }

    elements.addOne(ElementName("grader"), grader)

    val substituted = tagFindingRegex.replaceAllIn(markdown, substitute)
    val htmlTemplate: String = markdownRenderer.render(markdownParser.parse(substituted))

    Assessment(name = name, htmlTemplate = htmlTemplate, associatedFiles = associatedFiles.result,
      pageElements = elements.result(), tags = tags)
  }

  val tags: Tags[Assessment] = Tags.empty

  def testSolution(expected: Points = reachablePoints, changes: Seq[(PageElement, String)] = Seq.empty): AssessmentTest = new AssessmentTest {
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
      val (points, comment) = grader.grade(gradingContext)
      println(s"Resulting comments:\n${comment.map(comment => "* " + comment).mkString("\n")}")
      println(s"Resulting number of points: $points (expected points: $expected)")
      assert(points == expected)
    }
  }

  /** Run selftests of this assessment */
  def runTests()(using exceptionContext: ExceptionContext): Unit = {
    testSolution().runTest()

    for (method <- this.getClass.getMethods
         if method.getParameterCount == 0
         if classOf[AssessmentTest].isAssignableFrom(method.getReturnType))
      println(s"""Running test ${method.getName}:""")
      val test = method.invoke(this).asInstanceOf[AssessmentTest]
      test.runTest()
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
}