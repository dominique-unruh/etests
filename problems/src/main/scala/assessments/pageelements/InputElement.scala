package assessments.pageelements

import assessments.ElementName
import utils.Tag.Tags

/** Simple text input element. */
class InputElement(val name: ElementName,
                   val reference: String,
                   val tags: Tags[InputElement]) extends AnswerElement {
  override def renderHtml: String =
    s"""<input type="text" id="${name.jsElementId}" onInput='elementEvent("$name", {content: this.value})'/>"""
}
