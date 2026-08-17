package assessments.pageelements

import assessments.pageelements.RenderContext.studentAnswers
import assessments.pageelements.SolutionElement.{Feedback, Styling}
import assessments.pageelements.SolutionElement.Styling.explanation
import assessments.GradingContext.Outcome
import assessments.{Assessment, ElementName, FileMapBuilder, Html, HtmlConvertible, InterpolatedMarkdown, Points, SyntaxError}
import org.apache.commons.text.StringEscapeUtils.escapeHtml4
import play.api.libs.json.{JsNumber, JsObject, JsString, JsValue}
import utils.{IndentedInterpolator, Tag}
import utils.Tag.Tags
import utils.Utils.awaitResult

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
      val fb = feedback(
        context(RenderContext.problem),
        context.get(RenderContext.registrationNumber),
        context(RenderContext.studentAnswers)).awaitResult()
      val pointsHtml = fb.points match {
        case Some(points) => s"""<div class="solution-points">${escapeHtml4(points.decimalFractionString(precision = 2))} points</div>"""
        case None => ""
      }
      val outcomeHtml =
        if (fb.outcome == Outcome.unspecified) ""
        else s"""<div class="solution-outcome outcome-${escapeHtml4(fb.outcome.toString)}">${escapeHtml4(fb.outcome.toString)}</div>"""
      return Html(s"""<div class="solution solution-${escapeHtml4(styling.toString)}">$pointsHtml$outcomeHtml<div class="solution-body">${fb.text.html}</div></div>""")
    }
    Html(ind"""<etest-solution id="${name.htmlComponentNameEscaped}" styling="${escapeHtml4(styling.toString)}"></etest-solution>""")

  protected def feedback(assessment: Assessment, registrationNumber: Option[String], answers: Map[ElementName, String]): Future[Feedback]

  def pointsReached(assessment: Assessment, registrationNumber: Option[String], answers: Map[ElementName, String]): Future[Option[Points]] =
    feedback(assessment, registrationNumber, answers).map(_.points)

  override def getFeedback(assessment: Assessment, state: Map[ElementName, JsValue]): Future[JsObject] = {
    val registrationNumber = state.get(ElementName.registrationNumber).map(_.asInstanceOf[JsString].value)
    for (fb <- feedback(assessment, registrationNumber, assessment.webappStateToAnswers(state))) yield {
      val builder = Map.newBuilder[String, JsValue]
      builder.addOne(("text", JsString(fb.text.html)))
      for (points <- fb.points)
        builder.addOne(("points", JsNumber(points.toBigDecimal)))
      if (fb.outcome != Outcome.unspecified)
        builder.addOne(("outcome", JsString(fb.outcome.toString)))
      JsObject(builder.result())
    }
  }
}

object SolutionElement {
  enum Styling {
    case explanation
    case grading
  }

  case class Feedback(text: Html, points: Option[Points] = None, outcome: Outcome = Outcome.unspecified)
}
