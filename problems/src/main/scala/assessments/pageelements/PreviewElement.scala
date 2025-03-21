package assessments.pageelements

import assessments.{Assessment, ElementName}
import play.api.libs.json.{JsObject, JsString, JsValue}
import utils.IndentedInterpolator

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
    if (element.name == observed) {
      val content = payload.asInstanceOf[JsObject].value("content").asInstanceOf[JsString].value
      Seq(ElementAction(name, JsObject(Seq("preview" -> JsString(content)))))
    } else
      Seq.empty
  }
}
