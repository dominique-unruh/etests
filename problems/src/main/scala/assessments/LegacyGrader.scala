package assessments

import assessments.Comment.Kind
import assessments.ExceptionContext.initialExceptionContext
import assessments.LegacyGrader.{graderExecutionContext, logger}
import assessments.pageelements.SolutionElement.Feedback
import assessments.pageelements.SolutionElement.Styling.grading
import assessments.pageelements.{AnswerElement, DynamicElement, ElementAction, InputElement, RenderContext, SolutionElement}
import com.typesafe.scalalogging.Logger
import org.apache.commons.lang3.exception.ExceptionUtils
import org.apache.commons.text.StringEscapeUtils
import play.api.libs.json.{JsBoolean, JsNumber, JsObject, JsString, JsValue}
import utils.Utils
import utils.Utils.Timeout

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global

abstract class LegacyGrader(name: ElementName) extends SolutionElement(name = name, styling = grading) {
  def grade()(using context: GradingContext, exceptionContext: ExceptionContext): Unit
  lazy val reachablePoints: Points

  override def timeoutFeedback(assessment: Assessment, state: Map[ElementName, JsValue]): JsValue =
    JsObject(Map("processing" -> JsBoolean(true)))

  override protected def feedback(assessment: Assessment, state: Map[ElementName, JsValue]): Future[Feedback] =
    Future(getFeedbackSync(assessment, state))(using graderExecutionContext)

  private def getFeedbackSync(assessment: Assessment, state: Map[ElementName, JsValue]): Feedback = {
    given ExceptionContext = initialExceptionContext(s"Recomputing grading based on change of inputs in webapp")
    val registrationNumber = state.get(ElementName.registrationNumber) match
      case Some(regno) => regno.asInstanceOf[JsString].value match
        case "" => "NO_STUDENT"
        case regno => regno
      case None => "NO_STUDENT"
    logger.debug(s"Running grader for ${assessment.name}, $registrationNumber: $state")
    val answers = for (case element : AnswerElement <- assessment.pageElements.values) yield {
      state.get(element.name) match
        case Some(elementState) =>
          element.name -> elementState.asInstanceOf[JsString].as[String]
        case None => element.name -> ""
    }
    val context = GradingContext(answers.toMap, registrationNumber, reachablePoints)
    grade()(using context)
    val report = StringBuilder()
    val pointsString = context.points.decimalFractionString(precision = 2)
    report ++= Comment.seqToHtml(GradingContext.comments(using context).toSeq).html
    val feedback = Feedback(
      points = Some(context.points),
      text = Html(report.result()))
    feedback
  }
}

object LegacyGrader {
  private val logger = Logger[LegacyGrader]
  private val graderExecutionContext: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(10))
}
