package assessments

import assessments.Assessment.{tagEnd, tagStart}
import assessments.MarkdownAssessment.{markdownParser, markdownRenderer, tagFindingRegex}
import assessments.pageelements.PageElement
import exam.HardAssumptions
import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer

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
  }

  private def findMethod(elementName: ElementName) = {
/*
    var obj = this
    println(getClass.getName)
    if (obj.getClass.getName.endsWith("tions$"))
      val quantum = obj.getClass.getField("quantum").get(obj)
      println(quantum)
      println(obj.asInstanceOf[HardAssumptions.type].quantum)
      println(getClass.getFields.toSeq)
//    for (name <- elementName.names.dropRight(1))
//      ???
*/
    this.getClass.getMethod(elementName.toString.replace('.','$')).invoke(this)
  }

  final lazy val assessment: Assessment = {
    given ExceptionContext = ExceptionContext.initialExceptionContext(s"Markdown assessment $name")
    val seen = mutable.HashSet[ElementName]()
    //    val elements = ListBuffer[(ElementName, PageElement)]()
    val elements = SeqMap.newBuilder[ElementName, PageElement]
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
        case name =>
          val elementName = addPageElement(name)
          Regex.quoteReplacement(s"$tagStart$elementName$tagEnd")
      }
    }

    elements.addOne(ElementName("grader"), grader)

    val substituted = tagFindingRegex.replaceAllIn(markdown, substitute)
    val htmlTemplate: String = markdownRenderer.render(markdownParser.parse(substituted))

    Assessment(name, htmlTemplate, elements.result())
  }
}

object MarkdownAssessment {
  private val tagFindingRegex: Regex = """(?s)\{\{(.*?)}}""".r
  private val startTagRegex: Regex = """<(.*?)>""".r
  private val endTagRegex: Regex = """</(.*?)>""".r
  private val markdownParser = Parser.builder.build
  private val markdownRenderer = HtmlRenderer.builder.build
}