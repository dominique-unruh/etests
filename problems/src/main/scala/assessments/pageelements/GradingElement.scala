package assessments.pageelements

import assessments.ExceptionContext.initialExceptionContext
import assessments.GradingContext.{GradeBlockExit, bareGradeBlock, gradeBlock}
import assessments.pageelements.GradingElement.{graderExecutionContext, logger}
import assessments.pageelements.SolutionElement.Feedback
import assessments.{Answers, Assessment, Comment, ElementName, ExceptionContext, ExceptionWithContext, GradingContext, Html, HtmlConvertible, InterpolatedMarkdown, Points}
import com.typesafe.scalalogging.Logger
import play.api.libs.json.{JsString, JsValue}
import utils.Utils

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.boundary.Label

/** An inline grader: a [[SolutionElement]] (styled as a `grading` box) that carries both a rule
 * `text` and the `grader` block scoring it. Created via `grading(text, grader)` in
 * [[assessments.DynexiteDefaults]]; a problem usually has several, and their points are summed by
 * `Assessment`'s `PointsReached`.
 *
 * On each feedback request the `grader` runs inside a [[GradingContext.bareGradeBlock]] (with
 * `allowExitWithoutDone = true`, so it may finish with `done()` or by falling through; `abort()`
 * is rejected) under the `grading.timeout` via [[utils.Utils.runWithTimeoutFuture]]. The resulting
 * points and the rule text (plus any comments as a report) form this element's feedback. */
class GradingElement(name: ElementName,
                     val reachablePoints: Points,
                     text: GradingContext ?=> InterpolatedMarkdown[HtmlConvertible],
                     grader: (context: GradingContext, exceptionContext: ExceptionContext, label: Label[GradeBlockExit]) ?=> Unit)
  extends SolutionElement(name = name, styling = SolutionElement.Styling.grading) {

  override def computeFeedback(assessment: Assessment, registrationNumber: Option[String], answers: Answers): Future[SolutionElement.Feedback] = {
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
      import assessments.GradingContext.Outcome
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
      val pointsString = context.points.decimalFractionString(precision = 2)
      val report = Comment.seqToHtml(GradingContext.comments(using context).toSeq)
      val textAndReport = if (report.isEmpty) textAsHtml
      else textAsHtml + Html("<hr>") + report
      val feedback = Feedback(
        points = Some(context.points),
        text = textAndReport,
        outcome = context.outcome)
      feedback
    }.recover {
      case e: Exception => Feedback(text = textAsHtml, error = Some(e))
      case t: Throwable => Feedback(text = textAsHtml, error = Some(t.toString))
    }
  }
}


object GradingElement {
  private val logger = Logger[GradingElement]
  private val graderExecutionContext: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(10))
}