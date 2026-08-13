package assessments

import assessments.Assessment.feedbackTimeout
import assessments.pageelements.{AnswerElement, DynamicElement, Element, ElementAction, ErrorElement, ImageElement, InputElement, RenderContext, SolutionElement, StaticElement}
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
//    val renderContext = RenderContext(RenderContext.dynamic := false, RenderContext.studentAnswers := solution)
    val fileMapBuilder = DataUrlFileMapBuilder()
    def render(element: Element) = element.renderHtml(renderContext, fileMapBuilder)
    
    val body = renderHtml(render)
    assert(fileMapBuilder.result().isEmpty)

    // Add "extra data" to the rendering if exists
    val body2 = renderContext
      .get(RenderContext.studentAnswers)
      .flatMap(_.get(ElementName.extraData)) match {
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

  private object PointsReached extends DynamicElement {
    override val name: ElementName = ElementName.pointsReached

    private val processing = JsObject(Seq(("processing", JsBoolean(true))))

    override def getFeedback(assessment: Assessment, state: Map[ElementName, JsValue]): Future[JsObject] = {
      val registrationNumber = state.get(ElementName.registrationNumber).map(_.asInstanceOf[JsString].value)
      val pointIterFuture =
        Future.traverse(assessment.pageElements.values.collect { case e : SolutionElement => e })
          { _.pointsReached(assessment, registrationNumber, assessment.webappStateToAnswers(state)) }

      for (points <- pointIterFuture) yield {
        val sum = points.map(_.getOrElse(0 : Points)).sum
        JsObject(Seq(("points", JsNumber(sum.toBigDecimal))))
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

  def webappStateToAnswers(state: Map[ElementName, JsValue]) : Map[ElementName, String] = {
    val result = Map.newBuilder[ElementName, String]
    for (case element : AnswerElement <- pageElements.values) {
      val answer = state.get(element.name) match {
        case Some(value) => value.asInstanceOf[JsString].as[String]
        case None => ""
      }
      result += ((element.name, answer))
    }
    result.result()
  }
}

object Assessment {
  val feedbackTimeout = Duration("1 second")

  lazy val staticCSS: String = {
    val path = Path.of("problems/target/web/sass/main/stylesheets/static.css").toAbsolutePath
    Files.readString(path)
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
  val graderIncomplete = Tag[Assessment, Boolean](default=false)
}

class UserError(message: String) extends Exception(message)
/** @param message assumed to be HTML */
class SyntaxError(message: String) extends UserError(message)

