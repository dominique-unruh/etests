package assessments.pageelements

import MathPreviewElement.mathtextToLatex
import assessments.{Assessment, ElementName}
import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.PyQuote
import play.api.libs.json.{JsObject, JsString, JsValue}
import utils.Tag.Tags
import utils.{IndentedInterpolator, Tag}

/** Example of a preview that interprets the input as LaTeX math */
class MathPreviewElement(val name: ElementName,
                         val observed: ElementName,
                         val latexRenderer: String => String) extends PageElement {
  override val tags: Tag.Tags[MathPreviewElement.this.type] = Tags.empty
  override def renderHtml: String =
    ind"""<div style="font-weight: bold; border: solid 1 1 1 1;" id="${name.jsElementId}">Preview...</div><script>
         |  function ${name.jsElementCallbackName}(json) {
         |    console.log(json.preview);
         |    console.log(("#${name.jsElementId}"));
         |    let span = document.getElementById("${name.jsElementId}");
         |    console.log(span);
         |    MathJax.typesetClear([span]);
         |    span.textContent = json.preview;
         |    MathJax.typesetPromise([span]);
         |  }
         |</script>"""

  override def updateAction(assessment: Assessment, state: Map[ElementName, JsValue]): IterableOnce[ElementAction] = {
    val content = state(observed).asInstanceOf[JsObject].value("content").asInstanceOf[JsString].value
    val text = "\\[" + latexRenderer(content) + "\\]"
    Seq(ElementAction(name, JsObject(Seq("preview" -> JsString(text)))))
  }
}

object MathPreviewElement {
  private lazy val parse_expr = py.module("sympy.parsing.sympy_parser").parse_expr
  private lazy val toLatex: py.Dynamic = py.module("sympy.printing.latex").latex
  private lazy val symbol = py.module("sympy").Symbol
  private lazy val simplify = py.module("sympy").simplify

  def mathtextToLatex(math: String): String = {
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
  }
}