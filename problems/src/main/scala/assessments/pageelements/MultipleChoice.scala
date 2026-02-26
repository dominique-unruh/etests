package assessments.pageelements

import assessments.pageelements.MultipleChoice.Style.checkbox
import assessments.pageelements.MultipleChoice.{Style, checkboxLabel, notSelectedString}
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

  checkArguments()

  private def checkArguments() = {
    // Reference solution is one of the options
    if (!options.contains(reference))
      throw IllegalArgumentException(s"Multiple choice element $name has reference solution \"$reference\", but it should be one of ${options.keys.map(s => s"\"$s\"").mkString(", ")}")

    // checkboxLabel only for checkboxes
    if (tags.contains(checkboxLabel) && style != checkbox)
      throw IllegalArgumentException(s"Tag `checkboxLabel` only allowed for style `checkbox` (in multiple choice element $name)")

    // If it should be rendered as a checkbox, options are yes/no or true/false or similar
    if (style == checkbox) {
      val yesOptions = Seq("yes", "true", "1")
      val noOptions = Seq("no", "false", "0")
      if (options.size != 2)
        throw IllegalArgumentException(s"Multiple choice element $name has ${options.size} options, but is to be rendered as a checkbox.")
      val Seq(yes, no) = options.keysIterator.toSeq
      if (!yesOptions.contains(yes.toLowerCase))
        throw IllegalArgumentException(s"Multiple choice element $name has style checkbox and first option $yes, but should be one of ${yesOptions.mkString(", ")} (case-insensitive)")
      if (!noOptions.contains(no.toLowerCase))
        throw IllegalArgumentException(s"Multiple choice element $name has style checkbox and second option $no, but should be one of ${noOptions.mkString(", ")} (case-insensitive)")
    }
  }


  override def renderHtml: Html = style match
    case Style.select => renderHtmlSelect
    case Style.radio => renderHtmlRadio
    case Style.checkbox => renderHtmlCheckbox

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

  def renderHtmlCheckbox: Html = {
    val html = StringBuilder()
    val Seq(yes, no) = options.keysIterator.map(escapeHtml4).toSeq
    val label = tags(checkboxLabel).html match {
      case "" => ""
      case label => s""" <label for="${name.jsElementId}">$label</label>"""
    }
    println(s"LABEL ${tags(checkboxLabel)} $label")
    html ++= s"""<input type="checkbox" id="${name.jsElementId}" onchange="updateState('$name', {content: this.checked ? '$yes' : '$no'})"></input>$label\n"""
    html ++=
      ind"""<script>
           |  function ${name.jsElementCallbackName}(json) {
           |    let input = document.getElementById("${name.jsElementId}");
           |    input.checked = json.content == '$yes';
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
      case Style.checkbox =>
        Seq(ElementAction(this.name, JsObject(Seq("content" -> JsString(content)))))
    }
  }

}

object MultipleChoice {
  enum Style {
    case select
    case radio
    case checkbox
  }

  private val notSelectedString = "― not selected ―"

  val checkboxLabel: Tag[MultipleChoice, Html] = Tag(default = Html.empty)
}