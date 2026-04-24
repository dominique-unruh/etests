package assessments.pageelements

import assessments.{Assessment, ElementName, FileMapBuilder, Html, SyntaxError}
import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.PyQuote
import org.apache.commons.text.StringEscapeUtils
import org.apache.commons.text.StringEscapeUtils.escapeHtml4
import play.api.libs.json.{JsObject, JsString, JsValue}
import utils.Tag.Tags
import utils.{IndentedInterpolator, Tag}

import scala.util.Random

/** Example of a preview that interprets the input as LaTeX math */
class MathPreviewElement(val name: ElementName,
                         val observed: ElementName,
                         val latexRenderer: String => String) extends DynamicElement {
  override val tags: Tag.Tags[MathPreviewElement.this.type] = Tags.empty
  override def renderHtml(context: RenderContext, files: FileMapBuilder): Html =
    if (!context(RenderContext.dynamic))
      return renderStaticHtml(context, files)
    Html(ind"""<etest-math-preview id="${name.htmlComponentNameEscaped}"></etest-math-preview>""")

  private def contentToPreview(content: String) = {
    try {
      val math = latexRenderer(content)
      s"\\(${escapeHtml4(math)}\\)"
    } catch
      case e: SyntaxError =>
        s"""<span style="color:red; font-weight:bold;">${e.getMessage}</span>"""
      case e: Throwable =>
        e.printStackTrace()
        """<span style="color:red; font-weight:bold;">Internal error</span>"""
  }

  private def renderStaticHtml(context: RenderContext, files: FileMapBuilder): Html = {
    context.studentAnswer(observed) match {
      case Some(answer) =>
        val rendered = contentToPreview(answer)
        Html(s"""<span class="math-preview">$rendered</span>""")
      case None =>
        Html.empty
    }
  }

  override def getFeedback(assessment: Assessment, state: Map[ElementName, JsValue]): JsString = {
//    println((state, observed))
    val content = state(observed).asInstanceOf[JsString].value
    val text = contentToPreview(content)
    JsString(text)
  }
}
