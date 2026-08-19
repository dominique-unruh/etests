package assessments.pageelements

import assessments.pageelements.RenderContext.{catchExceptions, studentAnswers}
import assessments.pageelements.SolutionElement.{Feedback, Styling}
import assessments.pageelements.SolutionElement.Styling.explanation
import assessments.GradingContext.Outcome
import assessments.InterpolatedMarkdown.md
import assessments.{Answers, Assessment, ElementName, FileMapBuilder, Html, HtmlConvertible, InterpolatedMarkdown, Plaintext, Points, SyntaxError}
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
    JsObject(Seq("text" -> DynamicElement.hourglass))

  override def renderHtml(context: RenderContext, files: FileMapBuilder): Html =
    if (!context(RenderContext.dynamic)) {
      // Blank question sheet (e.g. a student printout): omit solution-only content entirely.
      if (!context.getOrElse(RenderContext.showSolutions, true))
        return Html("")
      val fb = {
        computeFeedback(
          context(RenderContext.problem),
          context.get(RenderContext.registrationNumber),
          context(RenderContext.studentAnswers)).awaitResult()
      }
      val pointsHtml = fb.points match {
        case Some(points) => s"""<div class="solution-points">${escapeHtml4(points.decimalFractionString(precision = 2))} points</div>"""
        case None => ""
      }
      val outcomeHtml =
        if (fb.outcome == Outcome.unspecified) ""
        else s"""<div class="solution-outcome outcome-${escapeHtml4(fb.outcome.toString)}">${escapeHtml4(fb.outcome.toString)}</div>"""
      for (error <- fb.error if !context(catchExceptions))
        error match {
          case e: Exception => throw e
          case s: String => throw new RuntimeException(s)
        }
      val errorHtml = fb.error match {
        case Some(error) => s"""<div class="solution-error">${escapeHtml4(SolutionElement.errorToString(error))}</div>"""
        case None => ""
      }
      return Html(s"""<div class="solution solution-${escapeHtml4(styling.toString)}">$pointsHtml$outcomeHtml$errorHtml<div class="solution-body">${fb.text.html}</div></div>""")
    }
    Html(ind"""<etest-solution id="${name.htmlComponentNameEscaped}" styling="${escapeHtml4(styling.toString)}"></etest-solution>""")

  def computeFeedback(assessment: Assessment, registrationNumber: Option[String], answers: Answers): Future[Feedback]

  def pointsReached(assessment: Assessment, registrationNumber: Option[String], answers: Answers): Future[Option[Points]] =
    computeFeedback(assessment, registrationNumber, answers).map(_.points)

  override def getFeedback(assessment: Assessment, state: Map[ElementName, JsValue]): Future[JsObject] = {
    val registrationNumber = state.get(ElementName.registrationNumber).map(_.asInstanceOf[JsString].value)
    val result = for (fb <- computeFeedback(assessment, registrationNumber, assessment.webappStateToAnswers(state))) yield {
      val builder = Map.newBuilder[String, JsValue]
      builder.addOne(("text", JsString(fb.text.html)))
      for (points <- fb.points)
        builder.addOne(("points", JsNumber(points.toBigDecimal)))
      if (fb.outcome != Outcome.unspecified)
        builder.addOne(("outcome", JsString(fb.outcome.toString)))
      for (error <- fb.error) {
        val errorHtml = md"**ERROR**: ${Plaintext(SolutionElement.errorToString(error))}".toHtml.flatten.html
        builder.addOne(("text", JsString(fb.text.html + errorHtml)))
        builder.addOne(("points", JsNumber(0)))
        builder.addOne(("outcome", JsString("error")))
      }
      JsObject(builder.result())
    }
    result.recover {
      case e : Throwable =>
        e.printStackTrace()
        JsObject(Seq(
          "text" -> JsString(md"**ERROR**: ${Plaintext(e.toString)}".toHtml.flatten.html),
          "points" -> JsNumber(0), "outcome" -> JsString("error")))
    }
  }
}

object SolutionElement {
  enum Styling {
    case explanation
    case grading
  }

  case class Feedback(text: Html, points: Option[Points] = None, outcome: Outcome = Outcome.unspecified, error: Option[String | Exception] = None)

  def errorToString(error: String | Exception): String = error match {
    case s: String => s
    case e: Exception => e.toString
  }
}
