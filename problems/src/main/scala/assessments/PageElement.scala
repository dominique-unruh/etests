package assessments

import assessments.Assessment.{markdownParser, markdownRenderer, templateRegex}
import assessments.ElementAction
import assessments.MathPreviewElement.mathtextToLatex
import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.PyQuote
import play.api.libs.json.{JsObject, JsString, JsValue}

/** Potentially interactive elements on an assessment page. */
trait PageElement {
  val name: ElementName
  def renderHtml: String
  def action(assessment: Assessment, payload: JsValue): (IterableOnce[ElementAction], Any) = (Seq.empty, ())
  def otherAction(assessment: Assessment, element: PageElement, data: Any, payload: JsValue): IterableOnce[ElementAction] = Seq.empty
}

case class ElementAction(element: ElementName, data: JsValue)

/** Simple text input element. */
class InputElement(val name: ElementName) extends PageElement {
  override def renderHtml: String =
    s"""<input type="text" id="${name.jsElementId}" onInput='elementEvent("$name", {content: this.value})'/>"""
}

/** Example of a preview that just repeats the text from the corresponding input element. */
class PreviewElement(val name: ElementName, val observed: ElementName) extends PageElement {
  override def renderHtml: String =
    ind"""<span style="font-weight: bold" id="${name.jsElementId}"/>Preview...<script>
         |  function ${name.jsElementCallbackName}(json) {
         |    console.log(json.preview);
         |    console.log(("#${name.jsElementId}"));
         |    console.log($$("#${name.jsElementId}"));
         |    document.getElementById("${name.jsElementId}").textContent = json.preview; }
         |</script>"""

  override def otherAction(assessment: Assessment, element: PageElement, data: Any, payload: JsValue): IterableOnce[ElementAction] = {
    val content = payload.asInstanceOf[JsObject].value("content").asInstanceOf[JsString]
    Seq(ElementAction(observed, JsObject(Seq("preview" -> content))))
  }
}

/** Example of a preview that interprets the input as LaTeX math */
class MathPreviewElement(val name: ElementName, val observed: ElementName) extends PageElement {
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

  override def otherAction(assessment: Assessment, element: PageElement, data: Any, payload: JsValue): IterableOnce[ElementAction] = {
    if (element.name == observed) {
      val content = payload.asInstanceOf[JsObject].value("content").asInstanceOf[JsString].value
      val text = "\\[" + mathtextToLatex(content) + "\\]"
      Seq(ElementAction(name, JsObject(Seq("preview" -> JsString(text)))))
    } else
      Seq.empty
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
