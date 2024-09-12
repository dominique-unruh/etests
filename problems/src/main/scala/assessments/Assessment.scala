package assessments

import assessments.Assessment.templateRegex
import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer

import scala.collection.mutable
import scala.util.matching.Regex
import play.api.libs.json.JsValue

import scala.collection.mutable.ListBuffer

class Assessment private (val htmlTemplate: String,
                          val pageElements: Map[ElementName, PageElement]) {
  checkValid()

  private def checkValid(): Unit = {
    for ((name,element) <- pageElements)
      assert(element.name == name)
  }

  def renderHtml(): String = {
    def substituted = mutable.HashSet[ElementName]()

    def substitute(matsch: Regex.Match): String = {
      val name = ElementName(matsch.group(1))
      assert(!substituted.contains(name))
      substituted.add(name)
      val pageElement = pageElements(name)
      val html = pageElement.renderHtml
      Regex.quoteReplacement(html)
    }

//    println(s"[$htmlTemplate]")
    val body = templateRegex.replaceAllIn(htmlTemplate, substitute)
    body
  }

  def elementEvent(elementName: ElementName, payload: JsValue): Seq[ElementAction] = {
    val element = pageElements(elementName)
    val reactions = ListBuffer[ElementAction]()
    val (reactions1, data) = element.action(this, payload)
    reactions ++= reactions1
    for (otherElement <- pageElements.values
         if otherElement ne element)
      reactions ++= otherElement.otherAction(this, element, data, payload)
    reactions.toSeq
  }
}

class UserError(message: String) extends Exception(message)
class SyntaxError(message: String) extends UserError(message)

object Assessment {
  private val tagFindingRegex: Regex = """\{\{(.*)}}""".r

  def fromMarkdown(markdown: String): Assessment = {
    val mapBuilder = mutable.HashMap[ElementName, PageElement]()

    def substitute(matsch: Regex.Match) = {
      val spec = matsch.group(1)
      val (nameRaw, code) = {
        val index = spec.indexOf(':')
        if (index == -1)
          throw new SyntaxError(s"Could not parse tag `$spec`, missing `:`")
        (spec.substring(0, index).trim, spec.substring(index + 1).trim)
      }
      val name = ElementName(nameRaw)
      val pageElement = code match {
        case "input" => InputElement(name)
        case "preview" => MathPreviewElement(name, name.dropRight(1))
      }
      assert(pageElement.name == name)
      if (mapBuilder.contains(name))
        throw new SyntaxError(s"Duplicate page element name `$name`")
      mapBuilder.addOne(name, pageElement)
      s"$tagStart$name$tagEnd"
    }

    val substituted = tagFindingRegex.replaceAllIn(markdown, substitute)
    val htmlTemplate: String = markdownRenderer.render(markdownParser.parse(substituted))
    new Assessment(htmlTemplate = htmlTemplate, pageElements = mapBuilder.toMap)
  }


  private val markdownParser = Parser.builder.build
  private val markdownRenderer = HtmlRenderer.builder.build
  val tagStart = '\uFFFA'
  val tagEnd = '\uFFFB'
  private val templateRegex = s"""$tagStart([0-9a-zA-Z._]+)$tagEnd""".r
}