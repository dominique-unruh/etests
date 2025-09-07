package assessments.pageelements

import assessments.pageelements.MultipleChoice.{Style, notSelectedString}
import assessments.{ElementName, Html, Points}
import org.apache.commons.text.StringEscapeUtils
import org.apache.commons.text.StringEscapeUtils.escapeHtml4
import play.api.libs.json.{JsNumber, JsObject, JsString}
import utils.{IndentedInterpolator, Tag, Utils}
import utils.Tag.Tags

import scala.collection.immutable.SeqMap
import scala.collection.mutable
import scala.util.Random

final class MultipleChoice(override val name: ElementName,
                           val options: SeqMap[String, String],
                           override val reference: String,
                           val style: MultipleChoice.Style,
                           override val tags: Tags[MultipleChoice] = Tags.empty)
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

  override def renderHtml: Html = style match
    case Style.select => renderHtmlSelect
    case Style.radio => renderHtmlRadio

  override def renderStaticHtml(answers: Map[ElementName, String]): Html = style match
    case Style.select => renderHtmlSelectStatic(selected = answers(name))
    case Style.radio => renderHtmlRadioStatic(selected = answers(name))

  def renderHtmlRadioStatic(selected: String): Html = {
    val html = StringBuilder()
    val groupName = name.jsElementId + Utils.uniqueId()
    html ++= selected
    html ++= s"""<fieldset>\n"""
    html ++= s"""<label><input disabled name="$groupName" type="radio" name="" value=""${if (selected=="") " checked" else ""}/>\n"""
    html ++= notSelectedString += '\n'
    html ++= "</label>\n"
    for ((optionName, optionText) <- options) {
      html ++= s"""<label><input disabled name="$groupName" type="radio" value="${escapeHtml4(optionName)}"${if (selected==optionName) " checked" else ""}/>\n"""
      html ++= optionText += '\n'
      html ++= "</label>\n"
    }
    html ++= "</fieldset>\n"

    Html(html.result())
  }

  def renderHtmlRadio: Html = {
    val html = StringBuilder()
    html ++= s"""<fieldset id="${name.jsElementId}">\n"""
    html ++= s"""<label><input type="radio" name="${name.jsElementId}" value="" onchange="updateState('$name', {content: this.value})"/>\n"""
    html ++= notSelectedString += '\n'
    html ++= "</label>\n"
    for ((optionName, optionText) <- options) {
      html ++= s"""<label><input type="radio" name="${name.jsElementId}" value="${escapeHtml4(optionName)}" onchange="updateState('$name', {content: this.value})"/>\n"""
      html ++= optionText += '\n'
      html ++= "</label>\n"
    }
    html ++= "</fieldset>\n"

    html ++=
      ind"""<script>
           |  function ${name.jsElementCallbackName}(json) {
           |    let radios = document.querySelectorAll('input[name="${name.jsElementId}"]');
           |    let radio = radios[json.index]
           |    radio.checked = true;
           |    updateState("$name", {content: radio.value});
           |  }
           |</script>""".stripMargin

    Html(html.result())
  }

  def renderHtmlSelectStatic(selected: String): Html = {
    val html = StringBuilder()
    html ++= s"""<select readonly>\n"""
    html ++= s"""<option value=""${if (selected=="") " selected" else ""}>$notSelectedString</option>\n"""
    for ((optionName, optionText) <- options)
      html ++= s"""<option value="${escapeHtml4(optionName)}"${if (selected==optionName) " selected" else ""}>$optionText</option>\n"""
    html ++= "</select>\n"
    Html(html.result())
  }

  def renderHtmlSelect: Html = {
    val html = StringBuilder()
    html ++= s"""<select id="${name.jsElementId}" onchange="updateState('$name', {content: this.value})">\n"""
    html ++= s"""<option value="">$notSelectedString</option>\n"""
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

    Html(html.result())
  }

  override def setAction(content: String): Seq[ElementAction] = {
    assert(content != null)
    style match {
      case Style.select =>
        Seq(ElementAction(this.name, JsObject(Seq("content" -> JsString(content)))))
      case Style.radio =>
        if (content == "")
          Seq(ElementAction(this.name, JsObject(Seq("index" -> JsNumber(0)))))
        else {
          val index = options.keys.toSeq.indexOf(content)
          val hd = options.keys.toSeq.head
          if (index < 0)
            throw RuntimeException(s"setAction(\"$content\") called, but options are ${options.keys.map(s => s"\"$s\"").mkString(", ")}")
          // index+1 because the webapp has "not selected" first
          Seq(ElementAction(this.name, JsObject(Seq("index" -> JsNumber(index + 1)))))
        }
    }
  }

}

object MultipleChoice {
  enum Style {
    case select
    case radio
  }

  private val notSelectedString = "― not selected ―"
}