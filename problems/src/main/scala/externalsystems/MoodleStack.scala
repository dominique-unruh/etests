package externalsystems

import assessments.Assessment
import assessments.pageelements.{ImageElement, InputElement, MathPreviewElement, PageElement, PreviewElement}
import org.apache.commons.text.StringEscapeUtils
import utils.Tag

import java.util.Base64
import scala.xml.*

object MoodleStack {

  enum InputType {
    case algebraic
    case string
    case matrix
  }

  enum InsertStars(val integerValue: Int) {
    /*
            <option value="0">Don't insert stars </option>
            <option value="1" selected="">Insert stars for implied multiplication only</option>
            <option value="2">Insert stars assuming single-character variable names</option>
            <option value="3">Insert stars for spaces only</option>
            <option value="4">Insert stars for implied multiplication and for spaces</option>
            <option value="5">Insert stars assuming single-character variables, implied and for spaces</option>
            <option value="6">Insert stars for implied multiplication, spaces, and no user-functions</option>
            <option value="7">Insert stars for implied multiplication, spaces, no user-functions and assuming single-character var</option>
     */
    /** Don't insert stars */
    case dontInsert extends InsertStars(0)
    /** Insert stars for implied multiplication only */
    case impliedMultiplication extends InsertStars(1)
  }

  /** Represents a Moodle/Stack input field */
  case class Input(typ: InputType,
                   name: String,
                   reference: String,
                   allowWords: Iterable[String],
                   extraOptions: Iterable[String],
                   insertStars: InsertStars,
                  ) {
    assert(name.nonEmpty)
    assert(reference.nonEmpty)
    assert(allowWords.forall(_.nonEmpty)) // TODO: Also: ASCII letters? No-spaces?
    assert(extraOptions.forall(_.nonEmpty)) // TODO: Also: ASCII letters? No-spaces?

    def xml: Elem =
      <input>
        <name>{name}</name>
        <type>{typ}</type>
        <tans>{reference}</tans>
        <boxsize>15</boxsize>
        <strictsyntax>1</strictsyntax>
        <insertstars>{insertStars.integerValue}</insertstars>
        <syntaxhint></syntaxhint>
        <syntaxattribute>0</syntaxattribute>
        <forbidwords></forbidwords>
        <allowwords>{allowWords.mkString(", ")}</allowwords>
        <forbidfloat>1</forbidfloat>
        <requirelowestterms>0</requirelowestterms>
        <checkanswertype>0</checkanswertype>
        <mustverify>1</mustverify>
        <showvalidation>1</showvalidation>
        <options>{extraOptions.mkString(", ")}</options>
      </input>
  }

  case class Question(name: String, questionText: String,
                      questionVariables: String = "",
                      files: Map[String, Array[Byte]] = Map.empty,
                      inputs: Seq[Input]) {
    def xml: Elem = {
      val filesXML = files map { (name, content) =>
        val base64 = Base64.getEncoder.encodeToString(content)
        <file name={name} path="/" encoding="base64">
          {base64}
        </file>
      } flatMap {
        Seq(Text("\n"), _)
      }

      val inputsXML = inputs.map(_.xml).flatMap {
        Seq(Text("\n"), _)
      }

      val result =
        <question type="stack">
          <name>
            <text>
              {name}
            </text>
          </name>
          <questiontext format="html">
            <text>
              {scala.xml.PCData(questionText)}
            </text>{filesXML}
          </questiontext>
          <generalfeedback format="html">
            <text></text>
          </generalfeedback>
          <defaultgrade>1</defaultgrade>
          <penalty>0.1</penalty>
          <hidden>0</hidden>
          <idnumber></idnumber>
          <stackversion>
            <text>2025040100</text>
          </stackversion>
          <questionvariables>
            <text>
              {scala.xml.PCData(questionVariables)}
            </text>
          </questionvariables>
          <specificfeedback format="html">
            <text></text>
          </specificfeedback>
          <questionnote format="moodle_auto_format">
            <text></text>
          </questionnote>
          <questiondescription format="moodle_auto_format">
            <text></text>
          </questiondescription>
          <questionsimplify>1</questionsimplify>
          <assumepositive>0</assumepositive>
          <assumereal>0</assumereal>
          <prtcorrect format="html">
            <text>
              <![CDATA[<p>Richtige Antwort, gut gemacht!</p>]]>
            </text>
          </prtcorrect>
          <prtpartiallycorrect format="html">
            <text>
              <![CDATA[<p>Ihre Antwort ist teilweise korrekt.</p>]]>
            </text>
          </prtpartiallycorrect>
          <prtincorrect format="html">
            <text>
              <![CDATA[<p>Falsche Antwort.</p>]]>
            </text>
          </prtincorrect>
          <decimals>.</decimals>
          <scientificnotation>*10</scientificnotation>
          <multiplicationsign>dot</multiplicationsign>
          <sqrtsign>1</sqrtsign>
          <complexno>i</complexno>
          <inversetrig>cos-1</inversetrig>
          <logicsymbol>lang</logicsymbol>
          <matrixparens>[</matrixparens>
          <variantsselectionseed></variantsselectionseed>{inputsXML}
        </question>

      result
    }
  }

  case class Quiz(questions: Question*) {
    def xml: Elem = <quiz>{questions.map{_.xml}}</quiz>
    def prettyXml: String =
      """<?xml version="1.0" encoding="UTF-8"?>""" + "\n" + PrettyPrinter(80, 2).format(xml)
  }

  def assessmentToQuestion(assessment: Assessment): Question = {
    val inputs = Seq.newBuilder[Input]
    val (questionText, explanation, gradingRules, associatedFiles) = assessment.renderHtml { (element, associatedFiles) =>
      element match {
        case pageElement: InputElement =>
          val name = pageElement.name.toString
          val input = Input(
            typ = pageElement.tags(moodleInputType),
            name = name,
            reference = pageElement.reference,
            allowWords = pageElement.tags(moodleAllowWords),
            extraOptions = pageElement.tags(moodleExtraOptions),
            insertStars = pageElement.tags(moodleInsertStars))
          inputs += input
          if (pageElement.tags(moodleNoPreview))
            s"[[input:$name]]"
          else
            s"[[input:$name]] [[validation:$name]]"
        case preview: MathPreviewElement =>
          val name = preview.observed.toString
          s"[[validation:$name]]"
        case ImageElement(png, basename) =>
          val name = associatedFiles.add(basename = basename, extension = "png", mimeType = "image/png", content = png)
          s"""<img src="@@PLUGINFILE@@/${StringEscapeUtils.escapeHtml4(name)}"/>"""
        case _ =>
          throw RuntimeException(s"Unknown page element (type ${element.getClass.getName}): $element")
      }
    }

    Question(name = assessment.name,
      questionText = questionText,
      inputs = inputs.result,
      files = associatedFiles.view.mapValues(_._2).toMap,
      questionVariables = assessment.tags(moodleQuestionVariables)
    )
  }

  object moodleAllowWords extends Tag[InputElement, Seq[String]](default=Seq.empty)
  object moodleQuestionVariables extends Tag[Assessment, String](default="")
  object moodleExtraOptions extends Tag[InputElement, Seq[String]](default=Seq.empty) {
    val allowEmpty = "allowEmpty"
    val simp = "simp"
  }
  object moodleNoPreview extends Tag[InputElement, Boolean](default=false)
  /** Which input field type should this be in Moodle/Stack? */
  object moodleInputType extends Tag[InputElement, InputType](default=InputType.algebraic)
  object moodleInsertStars extends Tag[InputElement, InsertStars](default=InsertStars.dontInsert)
}
