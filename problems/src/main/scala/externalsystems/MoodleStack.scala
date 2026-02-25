package externalsystems

import assessments.{Assessment, DefaultFileMapBuilder, Html}
import assessments.pageelements.{DynamicElement, ImageElement, InputElement, MathPreviewElement, MultipleChoice, StaticElement}
import org.apache.commons.text.StringEscapeUtils
import utils.Tag

import java.util.Base64
import scala.xml.*

object MoodleStack {
  /** A guess what kind of identifiers are OK in Moodle/Stack situations */
  private val identifierRegex = "^[a-zA-Z][a-zA-Z0-9_]*$".r.anchored

  enum InputType {
    case algebraic
    case string
    /** Matrix of fixed size. The size is automatically determined from the reference solution. */
    case matrix
    /** Matrix of variable size. */
    case varmatrix
    case dropdown
    case radio
    case checkbox
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
    /** Insert stars for implied multiplication only.
     * Warning: Currently unclear how to mimic that when grading and in our previews. */
    case impliedMultiplication extends InsertStars(1)
  }

  /** Represents a Moodle/Stack input field */
  case class Input(typ: InputType,
                   name: String,
                   reference: String,
                   forbidWords: Iterable[String],
                   allowWords: Iterable[String],
                   extraOptions: Iterable[String],
                   insertStars: InsertStars,
                  ) {
    assert(name.nonEmpty)
    assert(reference.nonEmpty)
    assert(allowWords.forall(identifierRegex.matches), allowWords)
    assert(extraOptions.forall(identifierRegex.matches))

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
        <forbidwords>{forbidWords.mkString(", ")}</forbidwords>
        <allowwords>{allowWords.mkString(", ")}</allowwords>
        <forbidfloat>1</forbidfloat>
        <requirelowestterms>0</requirelowestterms>
        <checkanswertype>0</checkanswertype>
        <mustverify>1</mustverify>
        <showvalidation>1</showvalidation>
        <options>{extraOptions.mkString(", ")}</options>
      </input>
  }

  case class Question(name: String, questionText: Html,
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

      val footerString = s"[Question name: $name]"
      val footer = s"""<p style="color: gray; font-size: smaller; text-align: right;">${StringEscapeUtils.escapeHtml4(footerString)}</p>"""

      val result =
        <question type="stack">
          <name>
            <text>
              {name}
            </text>
          </name>
          <questiontext format="html">
            <text>
              {scala.xml.PCData(questionText.html)}
              {footer}
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

  lazy val defaultForbiddenWords: Set[String] = {
    val letters = ('a' to 'z') ++ ('A' to 'Z')
    val result = Set.newBuilder[String]
    for (x <- letters)
      result += x.toString
      for (y <- letters)
        result += s"$x$y"
    result.result()
  }

  def inputElementToMoodle(inputElement: InputElement): Input = {
    val name = inputElement.name.toString
    val (allowWords, forbidWords) = inputElement.tags.get(moodleAllowWords) match
      case Some(allow) => (allow, (defaultForbiddenWords -- allow).toSeq.sortBy(_.toLowerCase))
      case None => (Seq.empty, Seq.empty)

    val typ = inputElement.tags(moodleInputType)
    if (Seq(InputType.radio, InputType.checkbox, InputType.dropdown).contains(typ))
      throw IllegalArgumentException(s"Input element $name cannot have tag moodleInputType = $typ")

    Input(
      typ = typ,
      name = name,
      reference = inputElement.tags.getOrElse(moodleReferenceSolution, inputElement.reference),
      forbidWords = forbidWords,
      allowWords = allowWords,
      extraOptions = inputElement.tags(moodleExtraOptions) appended MoodleExtraOptions.allowEmpty,
      insertStars = inputElement.tags(moodleInsertStars))
  }

  def multipleChoiceElementToMoodle(element: MultipleChoice): Input = {
    val name = element.name.toString

    if (element.tags.contains(moodleReferenceSolution))
      throw IllegalArgumentException(s"MultipleChoice element $name has tag moodleReferenceSolution. This is not allowed.")
    if (element.tags.contains(moodleInputType))
      throw IllegalArgumentException(s"MultipleChoice element $name has tag moodleInputType. This is not allowed.")

    val typ = element.style match {
      case MultipleChoice.Style.select => InputType.dropdown
      case MultipleChoice.Style.radio => InputType.radio
    }

    /** (Hopefully) produces a correctly escaped string literal for Maxima */
    def quote(s: String): String = {
      val sb = new StringBuilder("\"")
      for (c <- s) c match {
        case '"' => sb.append("\\\"")
        case '\\' => sb.append("\\\\")
        case '\n' => sb.append("\\n")
        case '\r' => sb.append("\\r")
        case '\t' => sb.append("\\t")
        case c => sb.append(c)
      }
      sb.append("\"").toString
    }

    val referenceSolutionSeq = element.options.zipWithIndex.map { case ((name, text), index) =>
      val selected = (index == 0)
      s"[${quote(name)}, $selected, ${quote(text)}]"
    }
    val referenceSolution = "[" + referenceSolutionSeq.mkString(", ") + "]"

    Input(
      typ = typ,
      name = name,
      reference = referenceSolution,
      forbidWords = Seq.empty,
      allowWords = Seq.empty,
      extraOptions = element.tags(moodleExtraOptions) appended MoodleExtraOptions.allowEmpty,
      insertStars = element.tags(moodleInsertStars))
  }

  def assessmentToQuestion(assessment: Assessment): Question = {
    val inputs = Seq.newBuilder[Input]
    val fileMapBuilder = DefaultFileMapBuilder("@@PLUGINFILE@@/")
    val (questionText, explanation, gradingRules) = assessment.renderHtml { element =>
      element match {
        case pageElement: InputElement =>
          val name = pageElement.name.toString
          inputs += inputElementToMoodle(pageElement)
          if (pageElement.tags(moodleNoPreview))
            Html(s"[[input:$name]]")
          else
            Html(s"[[input:$name]] [[validation:$name]]")
        case preview: MathPreviewElement =>
          val name = preview.observed.toString
          Html(s"[[validation:$name]]")
        case pageElement: MultipleChoice =>
          val name = pageElement.name.toString
          inputs += multipleChoiceElementToMoodle(pageElement)
          Html(s"[[input:$name]]")
        case element: StaticElement =>
          element.renderHtml(fileMapBuilder)
        case _ =>
          throw RuntimeException(s"Unknown page element (type ${element.getClass.getName}): $element")
      }
    }

    Question(name = assessment.name,
      questionText = questionText,
      inputs = inputs.result,
      files = fileMapBuilder.result().view.mapValues(_._2).toMap,
      questionVariables = assessment.tags(moodleQuestionVariables)
    )
  }

  val moodleAllowWords = Tag[InputElement, Seq[String]](default=Seq.empty)
  val moodleQuestionVariables = Tag[Assessment, String](default="")
  val moodleExtraOptions = Tag[InputElement, Seq[String]](default=Seq.empty)
  object MoodleExtraOptions {
    @deprecated("Automatically added")
    val allowEmpty = "allowEmpty"
    val simp = "simp"
  }
  @deprecated("Simply always add previews manually.")
  val moodleNoPreview = Tag[InputElement, Boolean](default=true)
  /** Which input field type should this be in Moodle/Stack? */
  val moodleInputType = Tag[InputElement, InputType](default=InputType.algebraic)
  /** Reference answer given to Moodle (if the one from the answer element is not accepted for some reason) */
  val moodleReferenceSolution = Tag[InputElement, String]()
  val moodleInsertStars = Tag[InputElement, InsertStars](default=InsertStars.dontInsert)
}


/*

Interesting (useful??) fact: One can override the STACK latex renderer with arbitary maxima code like this (in the question variables):

evs: eval_string;
evs("tex1(x) := \"something\";");
evs("tex(x,y) := \"something else\";");
%_stack_preamble_end;

*/