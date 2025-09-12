package assessments

import assessments.pageelements.{AnswerElement, DynamicElement, Element, ElementAction, ErrorElement, ImageElement, StaticElement}
import com.eed3si9n.eval.Eval
import org.apache.commons.text.StringEscapeUtils
import org.apache.commons.text.StringEscapeUtils.escapeHtml4
import org.commonmark.parser.Parser

import scala.collection.{SeqMap, mutable}
import scala.util.matching.Regex
import play.api.libs.json.{JsObject, JsValue}
import utils.Tag.Tags
import utils.{IndentedInterpolator, Utils}

import java.nio.file.{Files, Path}
import scala.collection.mutable.ListBuffer
import scala.util.boundary.break
import scala.util.{Random, Using, boundary}
import scala.xml.*

class Assessment (val name: String,
                  val questionTemplate: InterpolatedHtml[Element],
                  val explanationTemplate: InterpolatedHtml[Element],
                  val gradingRulesTemplate: InterpolatedHtml[Element],
                  val pageElements: SeqMap[ElementName, DynamicElement],
                  val reachablePoints: Points,
                  val tags: Tags[Assessment] = Tags.empty) {
  checkValid()

  private def checkValid(): Unit = {
    for ((name,element) <- pageElements)
      assert(element.name == name, (element.name, name))
  }

  def renderHtml(elementHtml: Element => Html): (Html, Html, Html) = {
    def substituted = mutable.HashSet[ElementName]()

    def substitute(interpolatable: Element): Html = {
      interpolatable match {
        case pageElement: DynamicElement =>
          val name = pageElement.name
          assert(!substituted.contains(name))
          substituted.add(name)
        case _ =>
      }
      elementHtml(interpolatable)
    }

//    val body = templateRegex.replaceAllIn(htmlTemplate, substitute)
    val body = questionTemplate.mapArgs(substitute).mkText
    val explanation = explanationTemplate.mapArgs(substitute).mkText
    val gradingRules = gradingRulesTemplate.mapArgs(substitute).mkText

    (body, explanation, gradingRules)
  }

  def renderStaticHtml(solution: Map[ElementName, String]): (Html, Html, Html) = {
    val fileMapBuilder = DataUrlFileMapBuilder()
    def render(element: Element) = element match {
      case element: DynamicElement =>
        element.renderStaticHtml(solution)
      case element: StaticElement =>
        element.renderHtml(fileMapBuilder)
    }

    val (body, explanation, gradingRules) = renderHtml(render)
    assert(fileMapBuilder.result().isEmpty)

    (body, explanation, gradingRules)
  }

  lazy val renderHtml: (Html, Html, Html, Map[String, (String, Array[Byte])]) = {
    val fileMapBuilder = DefaultFileMapBuilder("")
    def render(element: Element) = element match {
      case element: DynamicElement =>
        element.renderHtml
      case element: StaticElement =>
        element.renderHtml(fileMapBuilder)
    }

    val (body, explanation, gradingRules) = renderHtml(render)
    (body, explanation, gradingRules, fileMapBuilder.result())
  }

  def updateAction(state: JsObject): Seq[ElementAction] = {
    // TODO should only recalculate changed things
    val stateMap = state.value.map { (name, content) => (ElementName(name), content) }.toMap
    val actions =
      for (case element: DynamicElement <- pageElements.values;
           action <- element.updateAction(this, stateMap))
      yield action
    actions.toSeq
  }
  
  def referenceSolution: Map[ElementName, String] =
    Map.from(for (case (name: ElementName, element: AnswerElement) <- pageElements.iterator)
      yield name -> element.reference)
}

object Assessment {
  val htmlHeader: Html = Html(
    ind"""<meta charset="UTF-8">
         |<script>
         |  window.MathJax = {
         |    tex: {
         |      inlineMath: [['$$', '$$'], ['\\\\(', '\\\\)']],
         |      displayMath: [['$$$$', '$$$$'], ['\\\\[', '\\\\]']]
         |    }
         |  };
         |</script>
         |<script id="MathJax-script" async src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"></script>""")
}

class UserError(message: String) extends Exception(message)
/** @param message assumed to be HTML */
class SyntaxError(message: String) extends UserError(message)

