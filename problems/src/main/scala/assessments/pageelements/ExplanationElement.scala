package assessments.pageelements

import assessments.pageelements.SolutionElement.Feedback
import assessments.pageelements.SolutionElement.Styling.explanation
import assessments.*
import play.api.libs.json.JsValue

import scala.concurrent.Future

/** A [[SolutionElement]] that shows static explanation text (styled as an `explanation` box).
 * Created via `explain(...)` in [[assessments.DynexiteDefaults]]. It awards no points; its
 * feedback is just the rendered `text`. For a grading element that also scores, see [[GradingElement]]. */
class ExplanationElement(name: ElementName, text: InterpolatedMarkdown[HtmlConvertible]) extends SolutionElement(name = name, styling = explanation) {
  lazy val html: Html = text.toHtml.flatMapArgs(_.toHtml)

  override protected def feedback(assessment: Assessment, registrationNumber: Option[String], answers: Map[ElementName, String]): Future[Feedback] =
    Future.successful(Feedback(text = html))
}
