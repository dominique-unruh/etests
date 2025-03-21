package assessments.pageelements

import assessments.ElementName

/** Simple text input element. */
class InputElement(val name: ElementName,
                   val reference: String) extends AnswerElement[String] {
  override def renderHtml: String =
    s"""<input type="text" id="${name.jsElementId}" onInput='elementEvent("$name", {content: this.value})'/>"""
}
