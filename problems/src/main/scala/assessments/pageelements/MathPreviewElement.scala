package assessments.pageelements

import assessments.{Assessment, ElementName, Html}
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
         |    span.textContent = json.preview;
         |    MathJax.typesetPromise([span]);
         |  }
         |</script>""")

  override def renderStaticHtml(answers: Map[ElementName, String]): Html = {
    val math = latexRenderer(answers(observed))
    Html(
      s"""<span class="mathjax-render" style="background-color: lightgray;">\\(${escapeHtml4(math)}\\)</span>"""
    )
  }

  override def updateAction(assessment: Assessment, state: Map[ElementName, JsValue]): IterableOnce[ElementAction] = {
    val content = state(observed).asInstanceOf[JsObject].value("content").asInstanceOf[JsString].value
    val text = "\\(" + latexRenderer(content) + "\\)"
    Seq(ElementAction(name, JsObject(Seq("preview" -> JsString(text)))))
  }
}

object MathPreviewElement {
  private lazy val parse_expr = py.module("sympy.parsing.sympy_parser").parse_expr
  private lazy val toLatex: py.Dynamic = py.module("sympy.printing.latex").latex
  private lazy val symbol = py.module("sympy").Symbol
  private lazy val simplify = py.module("sympy").simplify

/*  def mathtextToLatex(math: String): String = {
    //    val sympy = py"""$module.parse_expr($math)"""
    try {
      val sympy = parse_expr(math)
      val target = py"""$symbol("x")**2"""
      val same1 = sympy == target
      val same2 = py"""$sympy == $target"""
      val same3 = py"""$simplify($sympy - $target) == 0"""
      println(("SAME?", same1, same2, same3))
      println(simplify(sympy))
      val latex = toLatex(sympy).toString
      latex
    } catch {
      case e: Exception =>
        e.printStackTrace()
        "PARSE ERROR"
    }
  }*/
}