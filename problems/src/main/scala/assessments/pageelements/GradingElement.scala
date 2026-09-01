package assessments.pageelements

import assessments.ExceptionContext.initialExceptionContext
import assessments.GradingContext.{DisplayOutcome, GraderOutcome}
import assessments.pageelements.GradingElement.{Feedback, errorToString, logger}
import assessments.pageelements.RenderContext.catchExceptions
import assessments.{Answers, Assessment, Comment, ElementName, Exam, ExceptionContext, ExceptionWithContext, FileMapBuilder, GradingContext, Html, HtmlConvertible, InterpolatedMarkdown, Markdown, Points}
import com.typesafe.scalalogging.Logger
import org.apache.commons.text.StringEscapeUtils.escapeHtml4
import play.api.libs.json.{JsNumber, JsObject, JsString, JsValue}
import utils.{IndentedInterpolator, Tag, Utils}
import utils.Tag.Tags
import utils.Utils.awaitResult

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

/** An inline grader: a [[SolutionElement]] that carries a rule `text` plus the `grader` block scoring
 * it, and renders live feedback (points, display badge, comments). Created via `grading(text,
 * reachablePoints, grader, ...)` in [[assessments.DynexiteDefaults]]; a problem usually has several.
 *
 * The `grader` returns a [[GraderOutcome]] ([[GraderOutcome.fires]] / [[GraderOutcome.firesPartially]]
 * / [[GraderOutcome.doesntFire]]); it does not award points directly. The points awarded follow from
 * the outcome, `reachablePoints`, and `negative`:
 *  - `fires` → ± `reachablePoints`;
 *  - `firesPartially(p)` → ± `p` (requires `0 < p < reachablePoints`);
 *  - `doesntFire` → 0;
 * where the sign is negative iff `negative` (a penalty rule that removes points).
 *
 * A rule is skipped entirely (verdict [[DisplayOutcome.notApplicable]], 0 points) if any rule named in
 * `unless` fired (fully or partially). The whole chain of a problem's rules is resolved together by
 * [[Assessment.gradeAll]], which orders them topologically by `unless`.
 *
 * `partial` marks the rule as awarding partial credit (only affects the display badge, not the
 * points). Points are summed by `Assessment`'s `PointsReached`.
 *
 * Shown only in the solution view. In the web app (dynamic render) it emits an `<etest-grading>` web
 * component that recomputes feedback as answers change; in a static render it evaluates the grader
 * once and inlines the result (omitted from blank question sheets).
 *
 * @param negative if `true`, this is a penalty rule: firing subtracts points instead of adding them.
 * @param partial  if `true`, firing this rule is only partial credit (affects the badge only).
 * @param unless   if any of these (higher-priority) rules fired, this rule is not evaluated
 *                 (`notApplicable`). */
class GradingElement(override val name: ElementName,
                     val reachablePoints: Points,
                     text: GradingContext ?=> InterpolatedMarkdown[HtmlConvertible],
                     grader: (context: GradingContext, exceptionContext: ExceptionContext) ?=> GraderOutcome,
                     val negative: Boolean = false,
                     val partial: Boolean = false,
                     val unless: Seq[ElementName] = Seq.empty)
  extends DynamicElement, SolutionElement {

  override val tags: Tag.Tags[GradingElement] = Tags.empty

  /** Points awarded for `outcome`, with the sign flipped for a `negative` (penalty) rule. Throws if a
   * `firesPartially(p)` violates `0 < p < reachablePoints`. */
  def signedPoints(outcome: GraderOutcome)(using ExceptionContext): Points = {
    val magnitude = outcome match {
      case GraderOutcome.fires => reachablePoints
      case GraderOutcome.firesPartially(p) =>
        if (!(p > Points.zero && p < reachablePoints))
          throw ExceptionWithContext(s"firesPartially($p) must satisfy 0 < points < $reachablePoints (grader $name)")
        p
      case GraderOutcome.doesntFire => Points.zero
    }
    if (negative) Points.zero - magnitude else magnitude
  }

  /** The display badge for a resolved outcome (`None` = suppressed by `unless`). */
  def displayFor(outcome: Option[GraderOutcome]): DisplayOutcome = outcome match {
    case None => DisplayOutcome.notApplicable
    case Some(GraderOutcome.doesntFire) => if (negative) DisplayOutcome.noPenalty else DisplayOutcome.incorrect
    case Some(GraderOutcome.fires) =>
      if (partial) DisplayOutcome.partial else if (negative) DisplayOutcome.penalized else DisplayOutcome.correct
    case Some(GraderOutcome.firesPartially(_)) => DisplayOutcome.partial
  }

  override def timeoutFeedback(assessment: Assessment, state: Map[ElementName, JsValue]): JsValue =
    JsObject(Seq("text" -> DynamicElement.hourglass))

  override def renderHtml(context: RenderContext, files: FileMapBuilder): Html =
    if (!context(RenderContext.dynamic)) {
      // Blank question sheet (e.g. a student printout): omit solution-only content entirely.
      if (!context.getOrElse(RenderContext.showSolutions, true))
        return Html("")
      val doCatch = context(catchExceptions)
      val feedbacks = context(RenderContext.problem).gradeAll(
        context(RenderContext.exam),
        context(RenderContext.studentAnswers),
        context.get(RenderContext.registrationNumber),
        catchExceptions = doCatch).awaitResult()
      val fb = feedbacks(name)
      for (error <- fb.error if !doCatch)
        error match {
          case e: Exception => throw e
          case s: String => throw new RuntimeException(s)
        }
      val pointsHtml = s"""<div class="grading-points">${escapeHtml4(fb.points.decimalFractionString(precision = 2))} points</div>"""
      val outcomeHtml = s"""<div class="grading-outcome outcome-${fb.display}" title="${escapeHtml4(fb.display.toString)}">${escapeHtml4(fb.display.glyph)}</div>"""
      val errorHtml = fb.error match {
        case Some(error) => s"""<div class="grading-error">${escapeHtml4(errorToString(error))}</div>"""
        case None => ""
      }
      return Html(s"""<div class="grading">$pointsHtml$outcomeHtml$errorHtml<div class="grading-body">${fb.text.html}</div></div>""")
    }
    Html(ind"""<etest-grading id="${name.htmlComponentNameEscaped}"></etest-grading>""")

  override def getFeedback(exam: Exam, assessment: Assessment,
                           state: Map[ElementName, JsValue]): Future[JsObject] = {
    val registrationNumber = state.get(ElementName.registrationNumber).map(_.asInstanceOf[JsString].value)
    val result = for (feedbacks <- assessment.gradeAll(exam, assessment.webappStateToAnswers(state), registrationNumber, catchExceptions = true))
      yield feedbacks(name).toJson
    result.recover {
      case e: Throwable =>
        e.printStackTrace()
        JsObject(Seq(
          "text" -> JsString(""),
          "error" -> JsString(e.toString),
          "points" -> JsNumber(0), "outcome" -> JsString(DisplayOutcome.error.toString)))
    }
  }

  /** Renders just this rule's `text` (no grader run). Used for the `notApplicable` / error feedback,
   * where the grader is not run or threw. */
  def renderText(assessment: Assessment, registrationNumber: Option[String], answers: Answers): Html = {
    given ExceptionContext = initialExceptionContext(s"Rendering grading rule text for $name")
    given GradingContext = GradingContext(answers.answers, registrationNumber.getOrElse("NO_STUDENT"), reachablePoints, assessment.sourceAssessment)
    text.toHtml.flatMapArgs(_.toHtml)
  }

  /** Runs the grader for this rule in isolation (ignoring `unless`), under the `grading.timeout`.
   * Returns the grader's [[GraderOutcome]] together with the rendered rule text (with any comments the
   * grader produced appended as a report). The returned future FAILS if the grader throws — callers
   * decide whether to surface that (webapp) or propagate it (tests, static render without
   * `catchExceptions`). */
  def runGrader(exam: Exam, assessment: Assessment, registrationNumber: Option[String],
                answers: Answers): Future[(GraderOutcome, Html)] = {
    given ExceptionContext = initialExceptionContext(s"Grading rule $name")
    given context: GradingContext = GradingContext(answers.answers, registrationNumber.getOrElse("NO_STUDENT"), reachablePoints, assessment.sourceAssessment)
    val textAsHtml = text.toHtml.flatMapArgs(_.toHtml)
    val duration = Utils.getSystemProperty("grading.timeout", "timeout for graders, e.g., 10s, 1m")
    logger.debug(s"Running grader $name, $registrationNumber: $answers")
    Utils.runWithTimeoutFuture(Duration(duration), s"${assessment.name}-$name-$registrationNumber") {
      val outcome = grader
      val report = Comment.seqToHtml(GradingContext.comments(using context).toSeq)
      val textAndReport = if (report.isEmpty) textAsHtml else textAsHtml + Html("<hr>") + report
      (outcome, textAndReport)
    }
  }

  /** If a grading exception applies to this grader for `registrationNumber`, produce the overriding
   * [[Feedback]] directly (skipping the grader). Well-formedness of the exceptions (problems and
   * graders exist) is already checked when they are loaded (see [[Exam.gradingExceptions]]). */
  def exceptionOverride(exam: Exam, assessment: Assessment, registrationNumber: Option[String],
                        answers: Answers): Option[Feedback] = {
    val overrideForThisGrader = for {
      regNr <- registrationNumber
      value <- exam.gradingExceptions().map.get((regNr, assessment.name, name))
    } yield value
    overrideForThisGrader.map { case (comment, points) =>
      given ExceptionContext = initialExceptionContext(s"Grading exception for $name")
      // Reconstruct an outcome from the overridden points, so `unless` / mutual-exclusion still work.
      val outcome =
        if (points == reachablePoints) GraderOutcome.fires
        else if (points <= Points.zero) GraderOutcome.doesntFire
        else GraderOutcome.firesPartially(points)
      Feedback(name = name, points = points, display = displayFor(Some(outcome)), outcome = Some(outcome),
        text = renderText(assessment, registrationNumber, answers) + Html("<hr>") + comment.toHtml)
    }
  }
}


object GradingElement {
  private val logger = Logger[GradingElement]

  /** Resolved feedback for one grading rule. `points` is the (signed) points awarded (0 when the rule
   * did not fire or is not applicable). `display` is the badge; `outcome` is the raw grader verdict
   * (`None` when the rule was not evaluated because an `unless` rule fired, or when the grader threw). */
  case class Feedback(name: ElementName, text: Html, points: Points, display: DisplayOutcome,
                      outcome: Option[GraderOutcome], error: Option[String | Exception] = None) {
    def fired: Boolean = outcome.exists(_.fired)

    def toJson: JsObject = {
      val builder = Map.newBuilder[String, JsValue]
      builder.addOne(("text", JsString(text.html)))
      builder.addOne(("points", JsNumber(points.toBigDecimal)))
      builder.addOne(("outcome", JsString(display.toString)))
      for (error <- error)
        builder.addOne(("error", JsString(errorToString(error))))
      JsObject(builder.result())
    }
  }

  /** Manual grade overrides, keyed by student registration number, assessment name, grading element to
   * override. Each override supplies a `comment` (rendered as the feedback body) and the `points` to
   * award. See [[GradingElement.exceptionOverride]]. */
  // TODO move somewhere else
  case class GradingExceptions(map: Map[(String, String, ElementName), (Markdown, Points)]) extends AnyVal
  object GradingExceptions {
    val empty = GradingExceptions(Map.empty)
  }

  def errorToString(error: String | Exception): String = error match {
    case s: String => s
    case e: Exception => e.toString
  }
}
