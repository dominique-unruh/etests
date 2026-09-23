package assessments

import assessments.Assessment.feedbackTimeout
import assessments.ExceptionContext.initialExceptionContext
import assessments.GradingContext.{DisplayOutcome, GraderOutcome}
import assessments.pageelements.GradingElement.{Feedback, GradingExceptions}
import assessments.pageelements.RenderContext.problem
import assessments.pageelements.{AnswerElement, DynamicElement, Element, ElementAction, ErrorElement, GradingElement, ImageElement, InputElement, RenderContext, StaticElement}
import com.eed3si9n.eval.Eval
import io.github.classgraph.ClassGraph
import org.apache.commons.text.StringEscapeUtils
import org.apache.commons.text.StringEscapeUtils.escapeHtml4
import org.commonmark.parser.Parser

import scala.collection.{SeqMap, mutable}
import scala.util.matching.Regex
import play.api.libs.json.{JsArray, JsBoolean, JsNumber, JsObject, JsString, JsValue}
import utils.Tag.Tags
import utils.{FutureCache, IndentedInterpolator, Memoize, Tag, Utils, memoized}

import java.io.{BufferedReader, InputStreamReader}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.collection.JavaConverters.{asScalaSet, mapAsScalaMapConverter}
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util.boundary.break
import scala.util.{Failure, Random, Success, Using, boundary}
import scala.xml.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class Assessment (val name: String,
                  val questionTemplate: InterpolatedHtml[Element],
                  val pageElements: SeqMap[ElementName, DynamicElement],
                  val reachablePoints: Points,
                  val sourceAssessment: MarkdownAssessment = null,
                  val tags: Tags[Assessment] = Tags.empty) {
  checkValid()

  private def checkValid(): Unit = {
    for ((name,element) <- pageElements)
      assert(element.name == name, (element.name, name))
  }

  def renderHtml(elementHtml: Element => Html): Html = {
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

    body
  }

  def renderStaticHtml(renderContext: RenderContext): Html = {
    val renderContext2 = renderContext `update` (problem := this)
//    val renderContext = RenderContext(RenderContext.dynamic := false, RenderContext.studentAnswers := solution)
    val fileMapBuilder = DataUrlFileMapBuilder()
    def render(element: Element) = element.renderHtml(renderContext2, fileMapBuilder)
    
    val body = renderHtml(render)
    assert(fileMapBuilder.result().isEmpty)

    // Add "extra data" to the rendering if exists
    val body2 = renderContext2
      .get(RenderContext.studentAnswers)
      .flatMap(_.answers.get(ElementName.extraData)) match {
      case Some(value) if value.trim.nonEmpty =>
        body + Html(s"""<div class="extra-data"><b>Extra data:</b> ${escapeHtml4(value)}""")
      case _ => body
    }

    body2
  }

  def renderHtml(exam: Exam): (Html, Map[String, (String, Array[Byte])]) = {
    val renderContext = RenderContext(RenderContext.dynamic := true, RenderContext.exam := exam)
    val fileMapBuilder = DefaultFileMapBuilder("")
    def render(element: Element) = element.renderHtml(renderContext, fileMapBuilder)

    val body = renderHtml(render)
    (body, fileMapBuilder.result())
  }

  def pointsReached(exam: Exam, answers: Answers, registrationNumber: Option[String]): Future[Points] =
    gradeAll(exam, answers, registrationNumber, catchExceptions = true).map { feedbacks =>
      feedbacks.values.map(_.points).foldLeft(0: Points)(_ + _)
    }

  /** Resolves ALL grading rules of this assessment together, honoring their `unless` dependencies, and
   * returns each rule's [[Feedback]] (badge, points, comments). The rules are ordered topologically by
   * `unless`; a rule is skipped (`notApplicable`, 0 points) if any rule it lists in `unless` fired.
   *
   * The result is memoized (per answers / student / grading-exceptions / `catchExceptions`), so the
   * many `etest-grading` components of one problem — plus the points total — all share a single pass
   * over the graders.
   *
   * @param catchExceptions if `true`, a grader that throws yields an `error` feedback (0 points) and
   *                        grading continues; if `false`, the throw propagates (fails the future). */
  def gradeAll(exam: Exam, answers: Answers, registrationNumber: Option[String],
               catchExceptions: Boolean): Future[Map[ElementName, Feedback]] =
    // Pass the grading exceptions explicitly so they are part of the memoization key (see
    // gradeAllMemoized): editing the exceptions CSV must invalidate a previously-cached result for
    // otherwise-identical arguments.
    gradeAllMemoized(exam, answers, registrationNumber, catchExceptions, exam.gradingExceptions())

  // Cache keys for gradeAllMemoized. `exam` is excluded (fixed per assessment, only feeds
  // gradingExceptions); the value class GradingExceptions is keyed by its underlying map.
  private given Memoize.Key[Exam] = Memoize.Key.constant
  private given Memoize.Key[Answers] = Memoize.Key.by(identity)
  private given Memoize.Key[Option[String]] = Memoize.Key.by(identity)
  private given Memoize.Key[GradingExceptions] = Memoize.Key.by(_.map)

  /** Per-instance memoization of [[gradeAll]], so the many `etest-grading` components of one problem —
   * plus the points total — all share a single pass over the graders. Keyed on answers / student /
   * gradingExceptions / catchExceptions. */
  @memoized
  private def gradeAllMemoized(exam: Exam, answers: Answers, registrationNumber: Option[String],
                               catchExceptions: Boolean, gradingExceptions: GradingExceptions)
      : Future[Map[ElementName, Feedback]] =
    gradeAllUncached(exam, answers, registrationNumber, catchExceptions)

  /** Topological order of the grading rules such that every rule comes after all rules it lists in
   * `unless`. Throws on an unknown `unless` target or a dependency cycle. */
  private def gradingOrder(elements: Seq[GradingElement])(using ExceptionContext): Seq[GradingElement] = {
    val byName = elements.map(e => e.name -> e).toMap
    for (e <- elements; u <- e.unless if !byName.contains(u))
      throw ExceptionWithContext(s"Grading rule ${e.name} lists unless=$u, but no such grading rule exists in this problem")
    val result = mutable.ListBuffer[GradingElement]()
    val visited = mutable.Set[ElementName]()
    val inProgress = mutable.Set[ElementName]()
    def visit(e: GradingElement): Unit = {
      if (visited(e.name)) return
      if (inProgress(e.name))
        throw ExceptionWithContext(s"Cycle in `unless` dependencies of grading rules (at ${e.name})")
      inProgress += e.name
      for (u <- e.unless) visit(byName(u))
      inProgress -= e.name
      visited += e.name
      result += e
    }
    elements.foreach(visit)
    result.toSeq
  }

  private def gradeAllUncached(exam: Exam, answers: Answers, registrationNumber: Option[String],
                               catchExceptions: Boolean): Future[Map[ElementName, Feedback]] = {
    given ExceptionContext = initialExceptionContext(s"Grading all rules of $name")
    val gradingElements = pageElements.values.collect { case e: GradingElement => e }.toSeq
    val order = gradingOrder(gradingElements)
    val init: Future[(Map[ElementName, Feedback], Set[ElementName])] =
      Future.successful((Map.empty, Set.empty))
    val folded = order.foldLeft(init) { (accFuture, e) =>
      accFuture.flatMap { case (results, fired) =>
        e.exceptionOverride(exam, this, registrationNumber, answers) match {
          case Some(fb) =>
            Future.successful((results + (e.name -> fb), if (fb.fired) fired + e.name else fired))
          case None if e.unless.exists(fired.contains) =>
            val fb = Feedback(e.name, e.renderText(this, registrationNumber, answers), Points.zero,
              DisplayOutcome.notApplicable, None)
            Future.successful((results + (e.name -> fb), fired))
          case None =>
            e.runGrader(exam, this, registrationNumber, answers).map { case (outcome, textHtml) =>
              val fb = Feedback(e.name, textHtml, e.signedPoints(outcome), e.displayFor(Some(outcome)), Some(outcome))
              (results + (e.name -> fb), if (outcome.fired) fired + e.name else fired)
            }.recover {
              case ex: Exception if catchExceptions =>
                Assessment.logger.warn(
                  s"Grader ${e.name} of $name threw for ${registrationNumber.getOrElse("NO_STUDENT")} " +
                    "(shown as an error box in the feedback)", ex)
                val fb = Feedback(e.name, e.renderText(this, registrationNumber, answers), Points.zero,
                  DisplayOutcome.error, None, Some(ex))
                (results + (e.name -> fb), fired)
            }
        }
      }
    }
    folded.map(_._1)
  }

  private object PointsReached extends DynamicElement {
    override val name: ElementName = ElementName.pointsReached

    private val processing = JsObject(Seq(("processing", JsBoolean(true))))

    override def getFeedback(exam: Exam, assessment: Assessment,
                             state: Map[ElementName, JsValue]): Future[JsObject] = {
      val pointsFuture = assessment.pointsReached(exam, assessment.webappStateToAnswers(state),
        state.get(ElementName.registrationNumber).map(_.asInstanceOf[JsString].value))
      for (points <- pointsFuture) yield {
        JsObject(Seq(("points", JsNumber(points.toBigDecimal))))
      }
    }

    override def timeoutFeedback(assessment: Assessment, state: Map[ElementName, JsValue]): JsObject =
      processing

    override val tags: Tags[PointsReached.this.type] = Tags.empty

    override def renderHtml(context: RenderContext, associatedFiles: FileMapBuilder): Html = ???
  }


  def getFeedback(exam: Exam, answer: JsObject): (JsObject, JsArray, Boolean) = {
    // TODO should only recalculate changed things
    val answerMap = answer.value.map { (name, content) => (ElementName.fromHtmlComponentName(name), content) }.toMap
    val elements =
      (pageElements.values.collect { case element: DynamicElement => element }.toSeq)
        `appended` PointsReached
    // The grading exceptions are part of the cache key: otherwise editing the exceptions CSV and
    // then reverting the answer to a previously-graded value would return the stale cached feedback.
    val gradingExceptions = exam.gradingExceptions()
    val feedbackFutures = for (element <- elements)
      yield FutureCache.evaluateFuture((this, element.name, answerMap, gradingExceptions))(element.getFeedback(exam, this, answerMap))
    val feedbackOptions = Utils.awaitSeq(feedbackFutures, feedbackTimeout)
    var timedOut = false
    val feedbacks = Seq.newBuilder[(String, JsValue)]
    val errors = Seq.newBuilder[JsString]
    for ((element, feedback) <- elements.zip(feedbackOptions))
      feedback match {
        case Some(Success(value)) =>
          feedbacks += element.name.htmlComponentName -> value
        case None =>
          timedOut = true
          feedbacks += element.name.htmlComponentName -> element.timeoutFeedback(this, answerMap)
        case Some(Failure(exception)) =>
          errors += JsString(Utils.exceptionMessage(exception))
      }

    (JsObject(feedbacks.result()), JsArray(errors.result()), timedOut)
  }
  
  def referenceSolution: Map[ElementName, String] =
    Map.from(for (case (name: ElementName, element: AnswerElement) <- pageElements.iterator)
      yield name -> element.reference)

  def webappStateToAnswers(state: Map[ElementName, JsValue]) : Answers = {
    val result = Map.newBuilder[ElementName, String]
    for (case element : AnswerElement <- pageElements.values) {
      val answer = state.get(element.name) match {
        case Some(value) => value.asInstanceOf[JsString].as[String]
        case None => ""
      }
      result += ((element.name, answer))
    }
    Answers(result.result())
  }
}

object Assessment {
  private val logger = com.typesafe.scalalogging.Logger[Assessment]

  val feedbackTimeout = Duration("1 second")

  /** The stylesheet embedded into every static (non-webapp) render — archives, exported PDFs, and
   *  the standalone HTML produced by [[htmlHeaderStatic]] (used by `ArchiveExam`, `TaskGradeEveryone`,
   *  and [[Exam]]'s static HTML). Compiled from `problems/src/main/assets/stylesheets/static.scss`.
   *
   *  sbt-web/sbt-sassify emits it as a *webjar* resource
   *  (`META-INF/resources/webjars/problems/<version>/stylesheets/static.css`), whose version segment
   *  is chosen by the build, so it is located by its trailing path rather than a fixed classpath
   *  path. (A hard-coded `/stylesheets/static.css` only ever matched stale leftover artifacts, or
   *  nothing on a clean build, causing static renders to ship outdated or missing styling.)
   *
   *  The webapp does not use this; it serves its own compiled `main.css`. */
  lazy val staticCSS: String = webjarResource("stylesheets/static.css")
  /** The MathJax configuration (delimiters, copyable/accessible LaTeX source), shared with the
   *  preview webapp, which serves the same file as `lib/problems/js/mathjax-config.js`. Compiled
   *  from `problems/src/main/assets/js/mathjax-config.js` and located on the classpath the same way
   *  as [[staticCSS]]. */
  lazy val mathjaxConfigJS: String = webjarResource("js/mathjax-config.js")

  /** Reads the sbt-web build output `path` (e.g. `stylesheets/static.css`) from the classpath.
   *
   *  Only the *webjar* copy under `META-INF/resources/webjars/` counts. sbt-web also leaves the
   *  same files at the classpath root of some build/run configurations, and those copies are not
   *  always regenerated: matching them too made the lookup depend on classpath order and could
   *  silently serve a stale file (which once shipped static HTML without the `mjx-copytext` rule,
   *  showing the hidden LaTeX source as ordinary text). */
  private def webjarResource(path: String): String = {
    val scanResult = new ClassGraph().acceptPaths("META-INF/resources").scan()
    try {
      val resources = scanResult.getResourcesWithLeafName(path.split('/').last)
        .filter(r => r.getPath.contains("META-INF/resources/webjars/") && r.getPath.endsWith(path))
      if (resources.isEmpty)
        throw new RuntimeException(
          s"Could not find $path below META-INF/resources/webjars on the classpath " +
            "(is the sbt-web build output present?)")
      resources.get(0).getContentAsString
    } finally scanResult.close()
  }
  lazy val htmlHeaderStatic: Html = Html(
    ind"""<meta charset="UTF-8">
         |<script>
         |  $mathjaxConfigJS
         |</script>
         |<style>
         |  $staticCSS
         |</style>
         |<script id="MathJax-script" async src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"></script>""")
}

class UserError(message: String) extends Exception(message)
/** @param message assumed to be HTML */
class SyntaxError(message: String) extends UserError(message)

