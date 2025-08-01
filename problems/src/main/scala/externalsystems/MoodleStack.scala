package externalsystems

import assessments.Assessment
import assessments.pageelements.{InputElement, PageElement}
import exam.y2025.iqc1.CnotConstruction.Image
import org.apache.commons.text.StringEscapeUtils
import utils.Tag

import java.util.Base64
import scala.xml.*

object MoodleStack {

  case class Input(name: String,
                   reference: String,
                   allowWords: Iterable[String] = Seq.empty,
                   extraOptions: Iterable[String] = Seq.empty,
                  ) {
    assert(name.nonEmpty)
    assert(reference.nonEmpty)
    assert(allowWords.forall(_.nonEmpty)) // TODO: Also: ASCII letters? No-spaces?
    assert(extraOptions.forall(_.nonEmpty)) // TODO: Also: ASCII letters? No-spaces?

    def xml: Elem =
      <input>
        <name>{name}</name>
        <type>algebraic</type>
        <tans>{reference}</tans>
        <boxsize>15</boxsize>
        <strictsyntax>1</strictsyntax>
        <insertstars>0</insertstars>
        <syntaxhint></syntaxhint>
        <syntaxattribute>0</syntaxattribute>
        <forbidwords></forbidwords>
        <allowwords>{allowWords.mkString(", ")}</allowwords>
        <forbidfloat>1</forbidfloat>
        <requirelowestterms>0</requirelowestterms>
        <checkanswertype>0</checkanswertype>
        <mustverify>1</mustverify>
        <showvalidation>1</showvalidation>
        <options>{allowWords.mkString(", ")}</options>
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
    val (questionText, associatedFiles) = assessment.renderHtml { (element, associatedFiles) =>
      element match {
        case pageElement: InputElement =>
          val name = pageElement.name.toString
          val input = Input(
            name = name,
            reference = pageElement.reference,
            allowWords = pageElement.tags(moodleAllowWords),
            extraOptions = pageElement.tags(moodleExtraOptions))
          inputs += input
          s"[[input:$name]] [[validation:$name]]"
        case Image(png, basename) =>
          val name = associatedFiles.add(basename = basename, extension = "png", mimeType = "image/png", content = png)
          s"""<img src="@@PLUGINFILE@@/${StringEscapeUtils.escapeHtml4(name)}"/>"""
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
  }
}
