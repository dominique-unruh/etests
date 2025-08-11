package assessments.pageelements

import assessments.{Assessment, ElementName}
import play.api.libs.json.JsValue
import utils.Tag.Tags

/** Simple text input element. */
class InputElement(val name: ElementName,
                   val reference: String,
                   val tags: Tags[InputElement]) extends AnswerElement {
  override def renderHtml: String =
    s"""<input type="text" id="${name.jsElementId}" onInput='updateState("$name", {content: this.value})'/>"""
}
