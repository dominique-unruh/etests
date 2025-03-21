package assessments

import assessments.MarkdownAssessment.{markdownParser, markdownRenderer, tagEnd, tagFindingRegex, tagStart}
import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer

import scala.collection.{SeqMap, mutable}
import scala.util.matching.Regex

abstract class MarkdownAssessment {
  val name: String = getClass.getName
  val markdown: String
  def grade(answers: Map[ElementName, String]): (Points, Seq[String])
  val points: Points

  final lazy val assessment: Assessment = {
    given ExceptionContext = ExceptionContext.initialExceptionContext(s"Markdown assessment $name")
    val seen = mutable.HashSet[ElementName]()
    //    val elements = ListBuffer[(ElementName, PageElement)]()
    val elements = SeqMap.newBuilder[ElementName, PageElement]

    def substitute(matsch: Regex.Match) = {
      val name = ElementName(matsch.group(1).strip())

      val pageElement: PageElement =
        try
          this.getClass.getMethod(name.toString).invoke(this) match
            case pageElement: PageElement => pageElement
            case result => throw ExceptionWithContext(s"Page element $name referenced in markdown assessment ${this.name}, but the corresponding method returns a ${result.getClass}", result)
        catch
          case _: NoSuchMethodException => throw ExceptionWithContext(s"Page element $name referenced in markdown assessment ${this.name}, but no corresponding method found")

      if (seen.contains(name))
        throw new SyntaxError(s"Duplicate page element name `$name`")
      seen.add(name)
      elements.addOne((name, pageElement))
      s"$tagStart$name$tagEnd"
    }

    elements.addOne(ElementName("grader"), new Grader(ElementName("grader")) {
      override def grade(answers: Map[ElementName, String]): (Points, Seq[String]) = MarkdownAssessment.this.grade(answers)
      override val points = MarkdownAssessment.this.points
    })

    val substituted = tagFindingRegex.replaceAllIn(markdown, substitute)
    val htmlTemplate: String = markdownRenderer.render(markdownParser.parse(substituted))

    Assessment(name, htmlTemplate, elements.result())
  }
}

object MarkdownAssessment {
  private val tagStart = '\uFFFA'
  private val tagEnd = '\uFFFB'
  private val tagFindingRegex: Regex = """(?s)\{\{(.*?)}}""".r
  private val markdownParser = Parser.builder.build
  private val markdownRenderer = HtmlRenderer.builder.build
}