package assessments

import assessments.Assessment.FileMapBuilder
import assessments.pageelements.{AnswerElement, Element, ElementAction, ErrorElement, ImageElement, PageElement}
import com.eed3si9n.eval.Eval
import org.apache.commons.text.StringEscapeUtils
import org.apache.commons.text.StringEscapeUtils.escapeHtml4
import org.commonmark.parser.Parser

import scala.collection.{SeqMap, mutable}
import scala.util.matching.Regex
import play.api.libs.json.{JsObject, JsValue}
import utils.Tag.Tags
import utils.Utils

import java.nio.file.{Files, Path}
import scala.collection.mutable.ListBuffer
import scala.util.boundary.break
import scala.util.{Random, Using, boundary}
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
      case _ => throw RuntimeException(s"Element of type ${element.getClass} not supported when rendering static HTML")
    }

    val (body, explanation, gradingRules, files) = renderHtml(render)
    assert(files.isEmpty)

    (body, explanation, gradingRules)
  }

  lazy val renderHtml: (Html, Html, Html, Map[String, (String, Array[Byte])]) = {
    def render(element: Element, associatedFiles: FileMapBuilder) = element match {
      case element: PageElement =>
        element.renderHtml
      // TODO ImageElement and ErrorElement could be handled the same as PageElement if we extend renderHtml to return associated files or something. Or take a FileMapBuilder.
      case ImageElement(png, basename) =>
        val name = associatedFiles.add(basename = basename, extension = "png", mimeType = "image/png", content = png)
        Html(s"""<img src="${escapeHtml4(name)}"/>""")
      case ErrorElement(message, files) =>
        val fileNames = files.toSeq map { (name, content) => associatedFiles.add(filename = name, mimeType = "text/plain", content = content) }
        val fileLinks = fileNames map { filename => s""" [<a href="${escapeHtml4(filename)}" target="_blank">${escapeHtml4(filename)}</a>]"""}
        Html(s"""<div style="background-color: red">ERROR: ${escapeHtml4(message)}${fileLinks}</div>""")
      case _ => throw RuntimeException(s"Element of type ${element.getClass} not supported when rendering dynamic HTML")
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
/** @param message assumed to be HTML */
class SyntaxError(message: String) extends UserError(message)

object Assessment {
//  val tagStart = '\uFFFA'
//  val tagEnd = '\uFFFB'
//  private val templateRegex = s"""$tagStart([0-9a-zA-Z._]+)$tagEnd""".r

  class FileMapBuilder {
    private val map = mutable.Map[String, (String, Array[Byte])]()
    def hasName(name: String): Boolean = map.contains(name)
    def result(): Map[String, (String, Array[Byte])] = map.toMap
    def add(filename: String, mimeType: String, content: Array[Byte]): String = {
      val (basename, extension) = Utils.splitExtFilename(filename)
      add(basename = basename, extension = extension, mimeType = mimeType, content = content)
    }
    def add(basename: String, extension: String, mimeType: String, content: Array[Byte]): String = {
      def freshName: String = boundary {
        val name = s"$basename.$extension"
        if (!hasName(name))
          break(name)
        for (i <- 1 until Int.MaxValue) {
          val namei = s"$basename$i.$extension"
          if (!hasName(namei))
            break(namei)
        }
        assert(false) // Unreachable unless there are Int.MaxValue many files
      }
      val name = freshName
      map.put(name, (mimeType, content))
      name
    }
  }
}