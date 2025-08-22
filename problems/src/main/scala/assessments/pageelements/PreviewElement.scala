package assessments.pageelements

import assessments.{Assessment, ElementName, Html}
import play.api.libs.json.{JsObject, JsString, JsValue}
import utils.Tag.Tags
import utils.{IndentedInterpolator, Tag}

/** Example of a preview that just repeats the text from the corresponding input element. */
class PreviewElement(val name: ElementName, val observed: ElementName) extends PageElement {
  override def renderHtml: Html =
    Html(ind"""<span style="font-weight: bold" id="${name.jsElementId}"/>Preview...<script>
         |  function ${name.jsElementCallbackName}(json) {
         |    console.log(json.preview);
         |    console.log(("#${name.jsElementId}"));
         |    console.log($$("#${name.jsElementId}"));
         |    document.getElementById("${name.jsElementId}").textContent = json.preview; }
         |</script>""")

  override val tags: Tag.Tags[PreviewElement.this.type] = Tags.empty

  override def renderStaticHtml(answers: Map[ElementName, String]): Html = ???

  override def updateAction(assessment: Assessment, state: Map[ElementName, JsValue]): IterableOnce[ElementAction] = {
    val content = state(observed).asInstanceOf[JsObject].value("content").asInstanceOf[JsString].value
    Seq(ElementAction(name, JsObject(Seq("preview" -> JsString(content)))))
  }
}
