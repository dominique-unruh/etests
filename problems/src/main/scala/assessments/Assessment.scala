package assessments

import assessments.Assessment.feedbackTimeout
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
import utils.{FutureCache, IndentedInterpolator, Tag, Utils}

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

  lazy val renderHtml: (Html, Map[String, (String, Array[Byte])]) = {
    val renderContext = RenderContext(RenderContext.dynamic := true)
    val fileMapBuilder = DefaultFileMapBuilder("")
    def render(element: Element) = element.renderHtml(renderContext, fileMapBuilder)

    val body = renderHtml(render)
    (body, fileMapBuilder.result())
  }

  def pointsReached(answers: Answers, registrationNumber: Option[String]): Future[Points] = {
    val pointIterFuture =
      Future.traverse(pageElements.values.collect { case e: GradingElement => e }) {
        _.pointsReached(this, registrationNumber, answers)
      }
    for (points <- pointIterFuture) yield
      points.map(_.getOrElse(0: Points)).sum
  }

  private object PointsReached extends DynamicElement {
    override val name: ElementName = ElementName.pointsReached

    private val processing = JsObject(Seq(("processing", JsBoolean(true))))

    override def getFeedback(assessment: Assessment, state: Map[ElementName, JsValue]): Future[JsObject] = {
      val pointsFuture = assessment.pointsReached(assessment.webappStateToAnswers(state),
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


  def getFeedback(answer: JsObject): (JsObject, JsArray, Boolean) = {
    // TODO should only recalculate changed things
    val answerMap = answer.value.map { (name, content) => (ElementName.fromHtmlComponentName(name), content) }.toMap
    val elements =
      (pageElements.values.collect { case element: DynamicElement => element }.toSeq)
        `appended` PointsReached
    val feedbackFutures = for (element <- elements)
      yield FutureCache.evaluateFuture((this, element.name, answerMap))(element.getFeedback(this, answerMap))
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
  lazy val staticCSS: String = {
    val scanResult = new ClassGraph().acceptPaths("META-INF/resources", "stylesheets").scan()
    try {
      val resources = scanResult.getResourcesWithLeafName("static.css")
        .filter(_.getPath.endsWith("stylesheets/static.css"))
      if (resources.isEmpty)
        throw new RuntimeException(
          "Could not find stylesheets/static.css on the classpath (is the sbt-sassify build output present?)")
      resources.get(0).getContentAsString
    } finally scanResult.close()
  }
  lazy val htmlHeaderStatic: Html = Html(
    ind"""<meta charset="UTF-8">
         |<script>
         |  window.MathJax = {
         |    tex: {
         |      inlineMath: [['$$', '$$'], ['\\\\(', '\\\\)']],
         |      displayMath: [['$$$$', '$$$$'], ['\\\\[', '\\\\]']]
         |    }
         |  };
         |</script>
         |<style>
         |  $staticCSS
         |</style>
         |<script id="MathJax-script" async src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"></script>""")
}

class UserError(message: String) extends Exception(message)
/** @param message assumed to be HTML */
class SyntaxError(message: String) extends UserError(message)

