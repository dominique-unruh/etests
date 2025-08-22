package assessments

import assessments.Assessment.FileMapBuilder
import assessments.pageelements.{AnswerElement, Element, ElementAction, ImageElement, PageElement}
import com.eed3si9n.eval.Eval
import org.apache.commons.text.StringEscapeUtils
import org.commonmark.parser.Parser

import scala.collection.{SeqMap, mutable}
import scala.util.matching.Regex
import play.api.libs.json.{JsObject, JsValue}
import utils.Tag.Tags
import utils.Utils

import java.nio.file.{Files, Path}
import scala.collection.mutable.ListBuffer
import scala.util.{Random, Using}
import scala.xml.*

class Assessment (val name: String,
                  val questionTemplate: InterpolatedHtml[Element],
                  val explanationTemplate: InterpolatedHtml[Element],
                  val gradingRulesTemplate: InterpolatedHtml[Element],
                  val pageElements: SeqMap[ElementName, PageElement],
                  val reachablePoints: Points,
                  val tags: Tags[Assessment] = Tags.empty) {
  checkValid()

  private def checkValid(): Unit = {
    for ((name,element) <- pageElements)
      assert(element.name == name, (element.name, name))
  }

  def renderHtml(elementHtml: (Element, FileMapBuilder) => Html):
             (Html, Html, Html, Map[String, (String, Array[Byte])]) = {
    def substituted = mutable.HashSet[ElementName]()
    val associatedFiles = new FileMapBuilder

    def substitute(interpolatable: Element): Html = {
      interpolatable match {
        case pageElement: PageElement =>
          val name = pageElement.name
          assert(!substituted.contains(name))
          substituted.add(name)
        case _ =>
      }
      elementHtml(interpolatable, associatedFiles)
    }

//    val body = templateRegex.replaceAllIn(htmlTemplate, substitute)
    val body = questionTemplate.mapArgs(substitute).mkText
    val explanation = explanationTemplate.mapArgs(substitute).mkText
    val gradingRules = gradingRulesTemplate.mapArgs(substitute).mkText

    (body, explanation, gradingRules, associatedFiles.result())
  }

  def renderStaticHtml(solution: Map[ElementName, String]): (Html, Html, Html) = {
    def render(element: Element, associatedFiles: FileMapBuilder) = element match {
      case element: PageElement =>
        element.renderStaticHtml(solution)
      case ImageElement(png, basename) =>
//        val name = associatedFiles.add(basename = basename, extension = "png", mimeType = "image/png", content = png)
        Html(s"""<img src="${Utils.dataUrl("image/png", png)}"/>""")
    }

    val (body, explanation, gradingRules, files) = renderHtml(render)
    assert(files.isEmpty)

    (body, explanation, gradingRules)
  }

  lazy val renderHtml: (Html, Html, Html, Map[String, (String, Array[Byte])]) = {
    def render(element: Element, associatedFiles: FileMapBuilder) = element match {
      case element: PageElement =>
        element.renderHtml
      case ImageElement(png, basename) =>
        val name = associatedFiles.add(basename = basename, extension = "png", mimeType = "image/png", content = png)
        Html(s"""<img src="${StringEscapeUtils.escapeHtml4(name)}"/>""")
    }

    renderHtml(render)
  }

  def updateAction(state: JsObject): Seq[ElementAction] = {
    // TODO should only recalculate changed things
    val stateMap = state.value.map { (name, content) => (ElementName(name), content) }.toMap
    val actions =
      for (case element: PageElement <- pageElements.values;
           action <- element.updateAction(this, stateMap))
      yield action
    actions.toSeq
  }
  
  def referenceSolution: Map[ElementName, String] =
    Map.from(for (case (name: ElementName, element: AnswerElement) <- pageElements.iterator)
      yield name -> element.reference)
}

class UserError(message: String) extends Exception(message)
class SyntaxError(message: String) extends UserError(message)

object Assessment {
//  val tagStart = '\uFFFA'
//  val tagEnd = '\uFFFB'
//  private val templateRegex = s"""$tagStart([0-9a-zA-Z._]+)$tagEnd""".r

  class FileMapBuilder {
    private val map = mutable.Map[String, (String, Array[Byte])]()
    def hasName(name: String): Boolean = map.contains(name)
    def result(): Map[String, (String, Array[Byte])] = map.toMap
    def add(basename: String, extension: String, mimeType: String, content: Array[Byte]): String = {
      def freshName: String = {
        val name = s"$basename.$extension"
        if (!hasName(name))
          return name
        for (i <- 1 until Int.MaxValue) {
          val namei = s"$basename$i.$extension"
          if (!hasName(namei))
            return namei
        }
        assert(false) // Unreachable unless there are Int.MaxValue many files
      }
      val name = freshName
      map.put(name, (mimeType, content))
      name
    }
  }
}