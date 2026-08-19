package assessments.pageelements

import assessments.ExceptionContext.initialExceptionContext
import assessments.GradingContext.{GradeBlockExit, Outcome, bareGradeBlock, gradeBlock}
import assessments.pageelements.GradingElement.{Feedback, errorToString, graderExecutionContext, logger}
import assessments.pageelements.RenderContext.catchExceptions
import assessments.{Answers, Assessment, Comment, ElementName, ExceptionContext, ExceptionWithContext, FileMapBuilder, GradingContext, Html, HtmlConvertible, InterpolatedMarkdown, Points}
import com.typesafe.scalalogging.Logger
import org.apache.commons.text.StringEscapeUtils.escapeHtml4
import play.api.libs.json.{JsNumber, JsObject, JsString, JsValue}
import utils.{IndentedInterpolator, Tag, Utils}
import utils.Tag.Tags
import utils.Utils.awaitResult

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.boundary.Label

/** An inline grader: a [[SolutionElement]] that carries both a rule `text` and the `grader` block
 * scoring it, and renders live feedback (points, outcome, comments). Created via `grading(text,
 * grader)` in [[assessments.DynexiteDefaults]]; a problem usually has several, and their points are
 * summed by `Assessment`'s `PointsReached`.
 *
 * On each feedback request the `grader` runs inside a [[GradingContext.bareGradeBlock]] (with
 * `allowExitWithoutDone = true`, so it may finish with `done()` or by falling through; `abort()`
 * is rejected) under the `grading.timeout` via [[utils.Utils.runWithTimeoutFuture]]. The resulting
 * points and the rule text (plus any comments as a report) form this element's feedback.
 *
 * Shown only in the solution view. In the web app (dynamic render) it emits an `<etest-grading>`
 * web component that recomputes feedback as answers change; in a static render it evaluates the
 * grader once and inlines the result (omitted entirely from blank question sheets). */
class GradingElement(override val name: ElementName,
                     val reachablePoints: Points,
                     text: GradingContext ?=> InterpolatedMarkdown[HtmlConvertible],
                     grader: (context: GradingContext, exceptionContext: ExceptionContext, label: Label[GradeBlockExit]) ?=> Unit)
  extends DynamicElement, SolutionElement {

  override val tags: Tag.Tags[GradingElement] = Tags.empty

  override def timeoutFeedback(assessment: Assessment, state: Map[ElementName, JsValue]): JsValue =
    JsObject(Seq("text" -> DynamicElement.hourglass))

  override def renderHtml(context: RenderContext, files: FileMapBuilder): Html =
    if (!context(RenderContext.dynamic)) {
      // Blank question sheet (e.g. a student printout): omit solution-only content entirely.
      if (!context.getOrElse(RenderContext.showSolutions, true))
        return Html("")
      val fb = computeFeedback(
        context(RenderContext.problem),
        context.get(RenderContext.registrationNumber),
        context(RenderContext.studentAnswers),
        catchExceptions = context(catchExceptions)).awaitResult()
      val pointsHtml = fb.points match {
        case Some(points) => s"""<div class="grading-points">${escapeHtml4(points.decimalFractionString(precision = 2))} points</div>"""
        case None => ""
      }
      val outcomeHtml =
        if (fb.outcome == Outcome.unspecified) ""
        else s"""<div class="grading-outcome outcome-${escapeHtml4(fb.outcome.toString)}">${escapeHtml4(fb.outcome.toString)}</div>"""
      for (error <- fb.error if !context(catchExceptions))
        error match {
          case e: Exception => throw e
          case s: String => throw new RuntimeException(s)
        }
      val errorHtml = fb.error match {
        case Some(error) => s"""<div class="grading-error">${escapeHtml4(errorToString(error))}</div>"""
        case None => ""
      }
      return Html(s"""<div class="grading">$pointsHtml$outcomeHtml$errorHtml<div class="grading-body">${fb.text.html}</div></div>""")
    }
    Html(ind"""<etest-grading id="${name.htmlComponentNameEscaped}"></etest-grading>""")

  def pointsReached(assessment: Assessment, registrationNumber: Option[String], answers: Answers): Future[Option[Points]] =
    computeFeedback(assessment, registrationNumber, answers, catchExceptions = false).map(_.points)

  override def getFeedback(assessment: Assessment, state: Map[ElementName, JsValue]): Future[JsObject] = {
    val registrationNumber = state.get(ElementName.registrationNumber).map(_.asInstanceOf[JsString].value)
    val result = for (fb <- computeFeedback(assessment, registrationNumber, assessment.webappStateToAnswers(state), catchExceptions = true)) yield {
      val builder = Map.newBuilder[String, JsValue]
      builder.addOne(("text", JsString(fb.text.html)))
      for (points <- fb.points)
        builder.addOne(("points", JsNumber(points.toBigDecimal)))
      if (fb.outcome != Outcome.unspecified)
        builder.addOne(("outcome", JsString(fb.outcome.toString)))
      for (error <- fb.error) {
        builder.addOne(("error", JsString(errorToString(error))))
        builder.addOne(("points", JsNumber(0)))
        builder.addOne(("outcome", JsString("error")))
      }
      JsObject(builder.result())
    }
    result.recover {
      case e : Throwable =>
        e.printStackTrace()
        JsObject(Seq(
          "text" -> JsString(""),
          "error" -> JsString(e.toString),
          "points" -> JsNumber(0), "outcome" -> JsString("error")))
    }
  }

  def computeFeedback(assessment: Assessment, registrationNumber: Option[String], answers: Answers,
                      catchExceptions: Boolean): Future[Feedback] = {
    given ExceptionContext = initialExceptionContext(s"Recomputing grading based on change of inputs in webapp")
    val duration = Utils.getSystemProperty("grading.timeout", "timeout for graders, e.g., 10s, 1m")
    logger.debug(s"Running grader $name, $registrationNumber: $answers")
    given GradingContext = GradingContext(answers.answers, registrationNumber.getOrElse("NO_STUDENT"), reachablePoints, assessment.sourceAssessment)
    val textAsHtml = text.toHtml.flatMapArgs(_.toHtml)
    Utils.runWithTimeoutFuture(Duration(duration), s"${assessment.name}-$name-${registrationNumber}") {
      val (exit, context) = bareGradeBlock(reachablePoints, allowExitWithoutDone = true) {
        grader }
      if (exit.abort) throw ExceptionWithContext("abort() not allowed in this grader")
      val points = context.points
      if (points > reachablePoints)
        throw ExceptionWithContext(s"Grader awarded $points points, more than the reachable $reachablePoints")
      context.outcome match {
        case Outcome.incorrect if points > Points.zero =>
          throw ExceptionWithContext(s"outcome=incorrect but $points points (> 0) awarded")
        case Outcome.correct | Outcome.partiallyCorrectFullPoints if points != reachablePoints =>
          throw ExceptionWithContext(s"outcome=${context.outcome} but $points points awarded (expected $reachablePoints)")
        case Outcome.partiallyCorrect if points < Points.zero =>
          throw ExceptionWithContext(s"outcome=partiallyCorrect but negative $points points awarded")
        case Outcome.notApplicable if points != Points.zero =>
          throw ExceptionWithContext(s"outcome=inapplicable but $points points awarded (expected 0)")
        case _ =>
      }
      val report = Comment.seqToHtml(GradingContext.comments(using context).toSeq)
      val textAndReport = if (report.isEmpty) textAsHtml
      else textAsHtml + Html("<hr>") + report
      Feedback(
        points = Some(context.points),
        text = textAndReport,
        outcome = context.outcome)
    }.recover {
      case e: Exception if catchExceptions => Feedback(text = textAsHtml, error = Some(e))
    }
  }
}


object GradingElement {
  private val logger = Logger[GradingElement]
  private val graderExecutionContext: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(10))

  case class Feedback(text: Html, points: Option[Points] = None, outcome: Outcome = Outcome.unspecified, error: Option[String | Exception] = None)

  def errorToString(error: String | Exception): String = error match {
    case s: String => s
    case e: Exception => e.toString
  }
}
