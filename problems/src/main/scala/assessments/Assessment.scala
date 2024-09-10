package assessments

import assessments.Assessment.templateRegex
import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer

import scala.collection.mutable
import scala.util.matching.Regex

class Assessment private (val htmlTemplate: String,
                          val pageElements: Map[ElementName, PageElement]) {
  checkValid()

  private def checkValid() = {
    for ((name,element) <- pageElements)
      assert(element.name == name)
  }

  def renderHtml(): String = {
    def substituted = mutable.HashSet[ElementName]()

    def substitute(matsch: Regex.Match): String = {
//      println(matsch.subgroups)
//      println(("G1", matsch.group(1)))
      val name = ElementName(matsch.group(1))
      assert(!substituted.contains(name))
      substituted.add(name)
      val pageElement = pageElements(name)
      val html = pageElement.renderHtml
//      println(html)
      Regex.quoteReplacement(html)
    }

    println(s"[$htmlTemplate]")
    val body = templateRegex.replaceAllIn(htmlTemplate, substitute)
    body
  }
}

object Assessment {
  def fromMarkdown(markdown: String): Assessment = {
    val mapBuilder = mutable.HashMap[ElementName, PageElement]()

    def substitute(spec: Regex.Match) = {
      val name = ElementName(spec.group(1).trim)
      val pageElement =
        if (name.names.last == "preview")
          PreviewElement(name, name.dropRight(1))
        else
          InputElement(name)
      mapBuilder.addOne(name, pageElement)
      s"$tagStart$name$tagEnd"
    }

    val substituted = """\{\{(.*)}}""".r.replaceAllIn(markdown, substitute)
    val htmlTemplate: String = markdownRenderer.render(markdownParser.parse(substituted))
    new Assessment(htmlTemplate = htmlTemplate, pageElements = mapBuilder.toMap)
  }

  private val markdownParser = Parser.builder.build
  private val markdownRenderer = HtmlRenderer.builder.build
  val tagStart = '\uFFFA'
  val tagEnd = '\uFFFB'
  private val templateRegex = s"""$tagStart([0-9a-zA-Z._]+)$tagEnd""".r
}