package assessments.pageelements

import assessments.{ElementName, Points}
import org.apache.commons.text.StringEscapeUtils
import org.apache.commons.text.StringEscapeUtils.escapeHtml4
import play.api.libs.json.{JsObject, JsString}
import utils.{IndentedInterpolator, Tag, Utils}
import utils.Tag.Tags

import scala.collection.immutable.SeqMap
import scala.collection.mutable

final class MultipleChoice(val name: ElementName,
                           val options: SeqMap[String, String],
                           val reference: String)
  extends AnswerElement {
  assert(options.contains(reference), (options, reference))

/*  private lazy val shortNames: SeqMap[String, String] = {
    val seen = mutable.HashSet[String]("notselected")
    val map = SeqMap.newBuilder[String, String]
    for (option <- options) {
      var shortName = ""
      var counter = 0
      while (shortName == "" || seen.contains(shortName))
      do {
        shortName = option.replace(' ', '-').filter(c => c.isLetterOrDigit || c == '-' || c == '_').take(20).toLowerCase
        if (counter > 0)
          shortName += s"_$counter"
        counter += 1
      }
      map += shortName -> option
      seen += shortName
    }
    map.result()
  }*/

  override val tags: Tag.Tags[MultipleChoice.this.type] = Tags.empty

/*  override def renderHtml: String = {
    val html = StringBuilder()
    html ++= s"""<select id="${name.jsElementId}" onchange="updateState("$name", {content: this.value})">\n"""
    for ((optionName, optionText) <- shortNames)
      html ++= s"""<option value="${escapeHtml4(optionName)}">$optionText</option>\n"""
    html ++= "</select>\n"
    html ++= ind"""<script>
               |  function ${name.jsElementCallbackName}(json) {
               |    let input = document.getElementById("${name.jsElementId}");
               |    console.log(input.value);
               |    input.value = json.content;
               |    updateState("$name", {content: json.content});
               |  }
               |</script>""".stripMargin

    html.result()
  }*/

  override def renderHtml: String = {
    val html = StringBuilder()
    html ++= s"""<select id="${name.jsElementId}" onchange="updateState('$name', {content: this.value})">\n"""
    html ++= """<option value="">― not selected ―</option>\n"""
    for ((optionName, optionText) <- options)
      html ++= s"""<option value="${escapeHtml4(optionName)}">$optionText</option>\n"""
    html ++= "</select>\n"
    html ++=
      ind"""<script>
           |  function ${name.jsElementCallbackName}(json) {
           |    let input = document.getElementById("${name.jsElementId}");
           |    console.log(input.value);
           |    input.value = json.content;
           |    updateState("$name", {content: json.content});
           |  }
           |</script>""".stripMargin

    html.result()
  }

  override def setAction(content: String): Seq[ElementAction] =
    assert(content != null)
    Seq(ElementAction(this.name, JsObject(Seq("content" -> JsString(content)))))
}