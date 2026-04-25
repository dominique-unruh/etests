package assessments.pageelements

import assessments.GradingContext.answers
import assessments.pageelements.MultipleChoice.Style.checkbox
import assessments.pageelements.MultipleChoice.{Style, checkboxLabel, notSelectedString}
import assessments.{Assessment, ElementName, FileMapBuilder, Html, Points}
import org.apache.commons.text.StringEscapeUtils
import org.apache.commons.text.StringEscapeUtils.escapeHtml4
import play.api.libs.json.{JsArray, JsNull, JsNumber, JsObject, JsString, JsValue, Json}
import utils.{IndentedInterpolator, Tag, Utils}
import utils.Tag.Tags

import scala.collection.immutable.SeqMap
import scala.collection.mutable
import scala.concurrent.Future

final class MultipleChoice(override val name: ElementName,
                           val options: SeqMap[String, String],
                           override val reference: String,
                           val style: MultipleChoice.Style,
                           override val tags: Tags[MultipleChoice] = Tags.empty)
  extends AnswerElement {

  checkArguments()

  lazy val yesNoAnswers: (String, String) = {
    assert(style == checkbox)
    val Seq(yes,no) = options.keysIterator.toSeq
    (yes, no)
  }
  lazy val yesAnswer: String = yesNoAnswers._1
  lazy val noAnswer: String = yesNoAnswers._2

  private def checkArguments(): Unit = {
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
      if (!yesOptions.contains(yesAnswer.toLowerCase))
        throw IllegalArgumentException(s"Multiple choice element $name has style checkbox and first option $yesAnswer, but should be one of ${yesOptions.mkString(", ")} (case-insensitive)")
      if (!noOptions.contains(noAnswer.toLowerCase))
        throw IllegalArgumentException(s"Multiple choice element $name has style checkbox and second option $noAnswer, but should be one of ${noOptions.mkString(", ")} (case-insensitive)")
    }
  }


  override def renderHtml(context: RenderContext, files: FileMapBuilder): Html = {
    if (context(RenderContext.dynamic)) {
      val optionsJson = Json.stringify(JsArray(options.toSeq.map((k,v) => JsArray(Seq(JsString(k), JsString(v))))))
//      val optionsJson = ujson.write(options.map(_.productIterator.toSeq))
      Html(
        ind"""<etest-multiple-choice id="${name.htmlComponentNameEscaped}" choicestyle="${escapeHtml4(style.toString)}"
             |    label="${tags(checkboxLabel).html}"
             |    options="${escapeHtml4(optionsJson)}">
             |</etest-multiple-choice>""")
    } else {
      val answer = context.studentAnswer(name)
      style match
        case Style.select => renderHtmlSelectStatic(selected = answer)
        case Style.radio => renderHtmlRadioStatic(selected = answer.getOrElse(""))
        case Style.checkbox => renderHtmlCheckboxStatic(selected = answer.getOrElse(""))
    }
  }

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

  def renderHtmlSelectStatic(selected: Option[String]): Html = {
    val html = StringBuilder()
    html ++= """<span class="static-select">"""
    for (((optionName, optionText), index) <- options.zipWithIndex) {
      if (index > 0) html ++= " | "
      val clazz = selected match {
        case None => "static-select-no-selection"
        case Some(`optionName`) => "static-select-yes"
        case Some(_) => "static-select-no"
      }
      html ++= s"""<span class="$clazz">$optionText</span>"""
    }
    html ++= "</span>"
    Html(html.result())
  }


  def renderHtmlCheckboxStatic(selected: String): Html = {
    val html = StringBuilder()
    val Seq(yes, no) = options.keysIterator.map(escapeHtml4).toSeq
    val label = tags(checkboxLabel).html match {
      case "" => ""
      case label => s""" <label for="${name.jsElementId}">$label</label>"""
    }
    val checked = if (selected == yes) " checked" else ""
//    println((selected, yes, no, checked))
    html ++= s"""<input disable type="checkbox" id="${name.jsElementId}"$checked></input>$label\n"""

    Html(html.result())
  }


  override def getFeedback(assessment: Assessment, state: Map[ElementName, JsValue]): Future[JsValue] = Future.successful(JsNull)
  override def timeoutFeedback(assessment: Assessment, state: Map[ElementName, JsValue]): JsValue = JsNull
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