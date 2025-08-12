package assessments

import assessments.pageelements.{ElementAction, PageElement}
import org.apache.commons.lang3.exception.ExceptionUtils
import org.apache.commons.text.StringEscapeUtils
import play.api.libs.json.{JsNumber, JsObject, JsString, JsValue}

abstract class Grader(val name: ElementName) extends PageElement {
  override def renderHtml: String = ""
  def grade(gradingContext: GradingContext): (Points, Seq[String])
  lazy val points: Points

  override def updateAction(assessment: Assessment, state: Map[ElementName, JsValue]): IterableOnce[ElementAction] = {
    val registrationNumber = "NO_STUDENT"
    val answers = state.map { (element, elementState) => (element, elementState("content").asInstanceOf[JsString].as[String]) }
    val gradingContext = GradingContext(answers, registrationNumber)
    try {
      val (points, reportLines) = grade(gradingContext)
      val report = StringBuilder()
      report ++= s"<p>Grading report for ${StringEscapeUtils.escapeHtml4(registrationNumber)}:</p>\n"
      val pointsString = points.decimalFractionString(precision = 2)
      if (points.isPreciseString(pointsString))
        report ++= s"<p>Points: $pointsString / ${assessment.reachablePoints.decimalFractionString}</p>\n"
      else
        report ++= s"<p>Points: $pointsString / ${assessment.reachablePoints.decimalFractionString}  (precise number: ${points.fractionHtml})</p>\n"
      report ++= "<ul>\n"
      for (line <- reportLines)
        report ++= s"  <li>${StringEscapeUtils.escapeHtml4(line)}</li>\n"
      report ++= "</ul>\n"
      Seq(ElementAction(name, JsObject(Map("points" -> JsString(points.decimalFractionString(2)), "report" -> JsString(report.result())))))
    } catch {
      case e : Throwable =>
        val message = ExceptionUtils.getStackTrace(e)
        Seq(ElementAction(ElementName.errordisplay, JsObject(Map("message" -> JsString(message)))))
    }
  }
}
