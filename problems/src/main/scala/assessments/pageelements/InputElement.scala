package assessments.pageelements

import assessments.pageelements.InputElement.{inputElementColumns, inputElementRows}
import assessments.{Assessment, ElementName, FileMapBuilder, Html}
import org.apache.commons.text.StringEscapeUtils
import org.apache.commons.text.StringEscapeUtils.escapeHtml4
import play.api.libs.json.{JsNull, JsObject, JsString, JsValue}
import utils.{IndentedInterpolator, Tag, Utils}
import utils.Tag.Tags

import scala.concurrent.Future

/** Simple text input element. */
case class InputElement(name: ElementName,
                        reference: String,
                        tags: Tags[InputElement]) extends AnswerElement {
  assert(tags(inputElementRows) > 0)
  assert(tags(inputElementColumns) > 0)
  private val useTextarea = tags(inputElementRows) > 1

  override def renderHtml(context: RenderContext, files: FileMapBuilder): Html = {
    if (!context(RenderContext.dynamic))
      return renderStaticHtml(context, files)
    Html(s"""<etest-text-input rows="${tags(inputElementRows)}" columns="${tags(inputElementColumns)}" id="${name.htmlComponentNameEscaped}"></etest-text-input>""")
  }

  private def renderStaticHtml(context: RenderContext, files: FileMapBuilder): Html = {
    val answer = context.studentAnswer(name).getOrElse("")
    if (useTextarea)
      Html(s"""<textarea rows="${tags(inputElementRows)}" cols="${tags(inputElementColumns)}" type="text" readonly>${escapeHtml4(answer)}</textarea>""")
    else
      Html(s"""<input type="text" size="${tags(inputElementColumns)}" readonly value="${escapeHtml4(answer)}"/>""")
  }

  override def getFeedback(assessment: Assessment, state: Map[ElementName, JsValue]): Future[JsValue] = Future.successful(JsNull)
  override def timeoutFeedback(assessment: Assessment, state: Map[ElementName, JsValue]): JsValue = JsNull
}

object InputElement {
  /** Width of the input element (in characters).
   * This does not prevent longer inputs, it just sets the size of the UI element. */
  val inputElementColumns: Tag[InputElement, Int] = Tag(default = 15)
  /** Number of rows of the input element */
  val inputElementRows: Tag[InputElement, Int] = Tag(default = 1)
}