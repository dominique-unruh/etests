package assessments

import assessments.Assessment.templateRegex
import assessments.pageelements.{ElementAction, PageElement}
import com.eed3si9n.eval.Eval
import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer

import scala.collection.{SeqMap, mutable}
import scala.util.matching.Regex
import play.api.libs.json.JsValue

import java.nio.file.{Files, Path}
import scala.collection.mutable.ListBuffer
import scala.util.{Random, Using}

class Assessment (val name: String,
                  val htmlTemplate: String,
                  val pageElements: SeqMap[ElementName, PageElement]) {
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
  val tagStart = '\uFFFA'
  val tagEnd = '\uFFFB'
  private val templateRegex = s"""$tagStart([0-9a-zA-Z._]+)$tagEnd""".r
}