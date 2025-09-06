package assessments.pageelements

import assessments.{Assessment, ElementName, Html, SyntaxError}
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
                         val latexRenderer: String => String) extends PageElement {
  override val tags: Tag.Tags[MathPreviewElement.this.type] = Tags.empty
  override def renderHtml: Html =
    Html(ind"""<span style="font-weight: bold; background-color: lightgray;" id="${name.jsElementId}">Preview...</span><script>
         |  function ${name.jsElementCallbackName}(json) {
         |    let span = document.getElementById("${name.jsElementId}");
         |    console.log(span);
         |    MathJax.typesetClear([span]);
         |    span.innerHTML = json.preview;
         |    MathJax.typesetPromise([span]);
         |  }
         |</script>""")

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

  override def renderStaticHtml(answers: Map[ElementName, String]): Html = {
    val rendered = contentToPreview(answers(observed))
    Html(s"""<span class="mathjax-render" style="background-color: lightgray;">$rendered</span>""")
  }

  override def updateAction(assessment: Assessment, state: Map[ElementName, JsValue]): IterableOnce[ElementAction] = {
    val content = state(observed).asInstanceOf[JsObject].value("content").asInstanceOf[JsString].value
    val text = contentToPreview(content)
    Seq(ElementAction(name, JsObject(Seq("preview" -> JsString(text)))))
  }
}
