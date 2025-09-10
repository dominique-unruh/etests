package assessments.pageelements

import assessments.pageelements.InputElement.inputElementRows
import assessments.{Assessment, ElementName, Html}
import org.apache.commons.text.StringEscapeUtils
import play.api.libs.json.{JsObject, JsString, JsValue}
import utils.{IndentedInterpolator, Tag, Utils}
import utils.Tag.Tags

/** Simple text input element. */
case class InputElement(val name: ElementName,
                        val reference: String,
                        val tags: Tags[InputElement]) extends AnswerElement {
  assert(tags(inputElementRows) > 0)
  private val useTextarea = tags(inputElementRows) > 1

  override def renderHtml: Html = {
    val tag = if (useTextarea) "textarea" else "input"
    Html(ind"""<$tag rows="${tags(inputElementRows)}" type="text" id="${name.jsElementId}" onInput='updateState("$name", {content: this.value})'></$tag><script>
         |  function ${name.jsElementCallbackName}(json) {
         |    let input = document.getElementById("${name.jsElementId}");
         |    console.log(input.value);
         |    input.value = json.content;
         |    updateState("$name", {content: json.content});
         |  }
         |</script>""")
  }

  override def renderStaticHtml(answers: Map[ElementName, String]): Html = {
    if (useTextarea)
      Html(s"""<textarea rows="${tags(inputElementRows)}" type="text" readonly>${StringEscapeUtils.escapeHtml4(answers(name))}</textarea>""")
    else
      Html(s"""<input type="text" readonly value="${StringEscapeUtils.escapeHtml4(answers(name))}"/>""")
  }
    
  override def setAction(content: String): Seq[ElementAction] =
    Seq(ElementAction(this.name, JsObject(Seq("content" -> JsString(content)))))
}

object InputElement {
  val inputElementRows: Tag[InputElement, Int] = Tag(default = 1)
}