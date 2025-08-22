package assessments.pageelements

import assessments.{Assessment, ElementName, Html}
import org.apache.commons.text.StringEscapeUtils
import play.api.libs.json.{JsObject, JsString, JsValue}
import utils.{IndentedInterpolator, Utils}
import utils.Tag.Tags

/** Simple text input element. */
class InputElement(val name: ElementName,
                   val reference: String,
                   val tags: Tags[InputElement]) extends AnswerElement {
  override def renderHtml: Html = {
    Html(ind"""<input type="text" id="${name.jsElementId}" onInput='updateState("$name", {content: this.value})'/><script>
         |  function ${name.jsElementCallbackName}(json) {
         |    let input = document.getElementById("${name.jsElementId}");
         |    console.log(input.value);
         |    input.value = json.content;
         |    updateState("$name", {content: json.content});
         |  }
         |</script>""")
  }

  override def renderStaticHtml(answers: Map[ElementName, String]): Html = Html(
    s"""<input type="text" readonly value="${StringEscapeUtils.escapeHtml4(answers(name))}"/>""")
    
  override def setAction(content: String): Seq[ElementAction] =
    Seq(ElementAction(this.name, JsObject(Seq("content" -> JsString(content)))))
}
