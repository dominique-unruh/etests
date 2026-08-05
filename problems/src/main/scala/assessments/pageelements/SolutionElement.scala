package assessments.pageelements

import assessments.pageelements.RenderContext.studentAnswers
import assessments.pageelements.SolutionElement.{Feedback, Styling}
import assessments.pageelements.SolutionElement.Styling.explanation
import assessments.{Assessment, ElementName, FileMapBuilder, Html, HtmlConvertible, InterpolatedMarkdown, Points, SyntaxError}
import org.apache.commons.text.StringEscapeUtils.escapeHtml4
import play.api.libs.json.{JsNumber, JsObject, JsString, JsValue}
import utils.{IndentedInterpolator, Tag}
import utils.Tag.Tags

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/** Represents an element that is shown **only** in the exam solution.
 * For example, can contain explanations about the problem, do grading, etc.
 **/
abstract class SolutionElement(val name: ElementName,
                               val styling: Styling,
                               val tags: Tag.Tags[SolutionElement] = Tags.empty) extends DynamicElement {
  override def timeoutFeedback(assessment: Assessment, state: Map[ElementName, JsValue]): JsValue =
    DynamicElement.hourglass

  override def renderHtml(context: RenderContext, files: FileMapBuilder): Html =
    if (!context(RenderContext.dynamic)) {
      val html: Html = ???
      return Html(s"""<div class="solution solution-${escapeHtml4(styling.toString)}">${html.html}</div>""")
    }
    Html(ind"""<etest-solution id="${name.htmlComponentNameEscaped}" styling="${escapeHtml4(styling.toString)}"></etest-solution>""")

  protected def feedback(assessment: Assessment, state: Map[ElementName, JsValue]): Future[Feedback]

  def pointsReached(assessment: Assessment, state: Map[ElementName, JsValue]): Future[Option[Points]] =
    feedback(assessment, state).map(_.points)

  override def getFeedback(assessment: Assessment, state: Map[ElementName, JsValue]): Future[JsObject] =
    for (fb <- feedback(assessment, state)) yield {
      val builder = Map.newBuilder[String, JsValue]
      builder.addOne(("text", JsString(fb.text.html)))
      for (points <- fb.points)
        builder.addOne(("points", JsNumber(points.toBigDecimal)))
      JsObject(builder.result())
    }
}

object SolutionElement {
  enum Styling {
    case explanation
    case grading
  }

  case class Feedback(text: Html, points: Option[Points] = None)
}
