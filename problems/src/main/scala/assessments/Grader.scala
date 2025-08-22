package assessments

import assessments.Comment.Kind
import assessments.ExceptionContext.initialExceptionContext
import assessments.Grader.logger
import assessments.pageelements.{AnswerElement, ElementAction, InputElement, PageElement}
import com.typesafe.scalalogging.Logger
import org.apache.commons.lang3.exception.ExceptionUtils
import org.apache.commons.text.StringEscapeUtils
import play.api.libs.json.{JsNumber, JsObject, JsString, JsValue}

abstract class Grader(val name: ElementName) extends PageElement {
  override def renderHtml: Html = Html.empty
  override def renderStaticHtml(answers: Map[ElementName, String]): Html = renderHtml
  
  def grade()(using context: GradingContext, exceptionContext: ExceptionContext): Unit
  lazy val reachablePoints: Points

  override def updateAction(assessment: Assessment, state: Map[ElementName, JsValue]): IterableOnce[ElementAction] = {
    given ExceptionContext = initialExceptionContext(s"Recomputing grading based on change of inputs in webapp")
    val registrationNumber = state.get(ElementName.registrationNumber) match
      case Some(regno) => regno.asInstanceOf[JsString].value match
        case "" => "NO_STUDENT"
        case regno => regno
      case None => "NO_STUDENT"
    val answers = for (case element : AnswerElement <- assessment.pageElements.values) yield {
      state.get(element.name) match
        case Some(elementState) => 
          element.name -> elementState("content").asInstanceOf[JsString].as[String]
        case None => element.name -> ""
    }
    val context = GradingContext(answers.toMap, registrationNumber)
    try {
      grade()(using context)
      val report = StringBuilder()
      report ++= s"<p>Grading report for ${StringEscapeUtils.escapeHtml4(registrationNumber)}:</p>\n"
      val pointsString = context.points.decimalFractionString(precision = 2)
      if (context.points.isPreciseString(pointsString))
        report ++= s"<p>Points: $pointsString / ${assessment.reachablePoints.decimalFractionString}</p>\n"
      else
        report ++= s"<p>Points: $pointsString / ${assessment.reachablePoints.decimalFractionString}  (precise number: ${context.points.fractionHtml})</p>\n"
      report ++= "<ul>\n"
      for (comment <- context.comments) {
        val line = comment.kind match
          case Kind.feedback => s"  <li>${comment.html}</li>\n"
          case Kind.debug => s"""  <li style="color:gray">${comment.html}</li>\n"""
          case Kind.warning => s"""  <li style="color:red">${comment.html}</li>\n"""
        report ++= line
      }
      report ++= "</ul>\n"
      Seq(ElementAction(name, JsObject(Map("points" -> JsString(context.points.decimalFractionString(2)), "report" -> JsString(report.result())))))
    } catch {
      case e : Throwable =>
        val message = ExceptionUtils.getStackTrace(e)
        Seq(ElementAction.error(message))
    }
  }
}

object Grader {
  private val logger = Logger[Grader]
}
