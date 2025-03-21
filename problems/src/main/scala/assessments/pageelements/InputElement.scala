package assessments.pageelements

import assessments.{ElementName, PageElement}

/** Simple text input element. */
class InputElement(val name: ElementName,
                   val reference: String) extends PageElement {
  override def renderHtml: String =
    s"""<input type="text" id="${name.jsElementId}" onInput='elementEvent("$name", {content: this.value})'/>"""
}
