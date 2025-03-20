package assessments

/** Simple text input element. */
class InputElement(val name: ElementName) extends PageElement {
  override def renderHtml: String =
    s"""<input type="text" id="${name.jsElementId}" onInput='elementEvent("$name", {content: this.value})'/>"""
}
