package assessments.pageelements

import assessments.{Assessment, ElementName, FileMapBuilder, Html, HtmlConvertible, InterpolatedMarkdown, SyntaxError}
import org.apache.commons.text.StringEscapeUtils.escapeHtml4
import play.api.libs.json.{JsString, JsValue}
import utils.{IndentedInterpolator, Tag}
import utils.Tag.Tags

import scala.concurrent.Future

/** Represents an element that is shown **only** in the exam solution.
 * For example, can contain explanations about the problem, do grading, etc.
 **/
class SolutionElement(val name: ElementName,
                      val text: InterpolatedMarkdown[HtmlConvertible],
                      val styling: String = "explanation",
                      val tags: Tag.Tags[SolutionElement] = Tags.empty) extends DynamicElement {
  override def timeoutFeedback(assessment: Assessment, state: Map[ElementName, JsValue]): JsValue =
    DynamicElement.hourglass

  lazy val html = text.toHtml.flatMapArgs(_.toHtml)

  override def renderHtml(context: RenderContext, files: FileMapBuilder): Html =
    if (!context(RenderContext.dynamic))
      return Html(s"""<div class="solution solution-${escapeHtml4(styling)}">${html.html}</div>""")
    Html(ind"""<etest-solution id="${name.htmlComponentNameEscaped}" styling="${escapeHtml4(styling)}"></etest-solution>""")

  override def getFeedback(assessment: Assessment, state: Map[ElementName, JsValue]): Future[JsString] =
    Future.successful(JsString(html.html))
}
