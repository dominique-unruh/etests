package assessments

import assessments.Assessment.FileMapBuilder
import assessments.MarkdownAssessment.Interpolatable
import assessments.pageelements.{ElementAction, PageElement}
import com.eed3si9n.eval.Eval
import exam.y2025.iqc1.CnotConstruction.Image
import org.apache.commons.text.StringEscapeUtils
import org.commonmark.parser.Parser

import scala.collection.{SeqMap, mutable}
import scala.util.matching.Regex
import play.api.libs.json.JsValue
import utils.Tag.Tags

import java.nio.file.{Files, Path}
import scala.collection.mutable.ListBuffer
import scala.util.{Random, Using}
import scala.xml.*

class Assessment (val name: String,
                  val htmlTemplate: InterpolatedString[Interpolatable],
                  val pageElements: SeqMap[ElementName, PageElement],
                  val tags: Tags[Assessment] = Tags.empty) {
  checkValid()

  private def checkValid(): Unit = {
    for ((name,element) <- pageElements)
      assert(element.name == name, (element.name, name))
  }

  def renderHtml(elementHtml: (Interpolatable, FileMapBuilder) => String) : (String, Map[String, (String, Array[Byte])]) = {
    def substituted = mutable.HashSet[ElementName]()
    val associatedFiles = new FileMapBuilder

    def substitute(interpolatable: Interpolatable): String = {
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
    val body = htmlTemplate.mapArgs(substitute).mkString
    (body, associatedFiles.result())
  }

  def renderHtml: (String, Map[String, (String, Array[Byte])]) = {
    def render(element: Interpolatable, associatedFiles: FileMapBuilder) = element match {
      case element: PageElement =>
        element.renderHtml
      case Image(png, basename) =>
        val name = associatedFiles.add(basename = basename, extension = "png", mimeType = "image/png", content = png)
        s"""<img src="${StringEscapeUtils.escapeHtml4(name)}"/>"""
    }

    renderHtml(render)
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