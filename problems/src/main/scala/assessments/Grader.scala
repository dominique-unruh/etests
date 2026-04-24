package assessments

import assessments.Comment.Kind
import assessments.ExceptionContext.initialExceptionContext
import assessments.Grader.logger
import assessments.pageelements.{AnswerElement, DynamicElement, ElementAction, InputElement, RenderContext}
import com.typesafe.scalalogging.Logger
import org.apache.commons.lang3.exception.ExceptionUtils
import org.apache.commons.text.StringEscapeUtils
import play.api.libs.json.{JsNumber, JsObject, JsString, JsValue}
import utils.Utils

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

abstract class Grader(val name: ElementName) extends DynamicElement {
  override def renderHtml(context: RenderContext, files: FileMapBuilder): Html = Html.empty
  
  def grade()(using context: GradingContext, exceptionContext: ExceptionContext): Unit
  lazy val reachablePoints: Points

  override def timeoutFeedback(assessment: Assessment, state: Map[ElementName, JsValue]): JsValue =
    JsObject(Map("error" -> JsString("Timeout")))

  override def getFeedback(assessment: Assessment, state: Map[ElementName, JsValue]): Future[JsObject] = Future {
    given ExceptionContext = initialExceptionContext(s"Recomputing grading based on change of inputs in webapp")
    val registrationNumber = state.get(ElementName.registrationNumber) match
      case Some(regno) => regno.asInstanceOf[JsString].value match
        case "" => "NO_STUDENT"
        case regno => regno
      case None => "NO_STUDENT"
    val answers = for (case element : AnswerElement <- assessment.pageElements.values) yield {
      state.get(element.name) match
        case Some(elementState) => 
          element.name -> elementState.asInstanceOf[JsString].as[String]
        case None => element.name -> ""
    }
    val context = GradingContext(answers.toMap, registrationNumber, reachablePoints)
    try {
      grade()(using context)
      val report = StringBuilder()
      report ++= s"<p>Grading report for ${StringEscapeUtils.escapeHtml4(registrationNumber)}:</p>\n"
      val pointsString = context.points.decimalFractionString(precision = 2)
      if (context.points.isPreciseString(pointsString))
        report ++= s"<p>Points: $pointsString / ${assessment.reachablePoints.decimalFractionString}</p>\n"
      else
        report ++= s"<p>Points: $pointsString / ${assessment.reachablePoints.decimalFractionString}  (precise number: ${context.points.fractionHtml})</p>\n"
      report ++= Comment.seqToHtml(GradingContext.comments(using context).toSeq).html
      JsObject(Map(
        "points" -> JsString(context.points.decimalFractionString(2)),
        "report" -> JsString(report.result())))
    } catch {
      case e: Throwable =>
        JsObject(Map("error" -> JsString(Utils.exceptionMessage(e))))
    }
  }
}

object Grader {
  private val logger = Logger[Grader]
}
