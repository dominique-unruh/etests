package assessments

import assessments.Assessment.{markdownParser, markdownRenderer, templateRegex}

trait PageElement {
  val name: ElementName
  def renderHtml: String
}

class InputElement(val name: ElementName) extends PageElement {
  override def renderHtml: String =
    s"""<input type="text" id="${name.jsElementId}" onInput='elementAction("$name", {content: this.value})'/>"""
}

class PreviewElement(val name: ElementName, val observed: ElementName) extends PageElement {
  override def renderHtml: String =
    ind"""<input type="text" readonly id="${name.jsElementId}"/><script>
         |  function ${name.jsElementCallbackName}(json) {
         |    console.log(json.preview);
         |    console.log(("#${name.jsElementId}"));
         |    console.log($$("#${name.jsElementId}"));
         |    document.getElementById("${name.jsElementId}").value = json.preview; }
         |</script>"""
}
