package externalsystems

import assessments.pageelements.MultipleChoice.{Style, checkboxLabel}
import assessments.{Assessment, DefaultFileMapBuilder, DummyExam, Html}
import assessments.pageelements.{DynamicElement, ImageElement, InputElement, MathPreviewElement, MultipleChoice, RenderContext, SolutionElement, StaticElement}
import org.apache.commons.text.StringEscapeUtils
import org.apache.commons.text.StringEscapeUtils.escapeHtml4
import utils.{Tag, Utils}

import java.util.Base64
import scala.xml.*

object MoodleStack {
  /** A guess what kind of identifiers are OK in Moodle/Stack situations */
  private val identifierRegex = "^[a-zA-Z][a-zA-Z0-9_]*$".r.anchored

  /** Extra options are either a plain identifier (e.g. `simp`) or a `key:value` form (e.g. `validator:myfunc`). */
  private val optionRegex = "^[a-zA-Z][a-zA-Z0-9_]*(:[a-zA-Z][a-zA-Z0-9_]*)?$".r.anchored

  /** Extracts the name of the variable/function defined by a Maxima statement of the form
   * `name : ...` or `name(args) := ...` (returns `None` if it does not have that shape). */
  private def definedVariableName(definition: String): Option[String] =
    "^\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*(\\([^)]*\\))?\\s*:=?".r.findFirstMatchIn(definition).map(_.group(1))

  enum InputType {
    case algebraic
    /** Input is a raw string */
    case string
    /** Input is a number (appears to have no effect at least in Dynexite) */
    case numerical
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

  /** A named Maxima function together with the definitions it (and its sub-definitions) need. Used
   * e.g. as a [[moodleValidation]] validator (see [[MoodleValidationHelpers]]).
   *
   * @param name    the name of the Maxima function.
   * @param dependencies other function definitions this one depends on.
   * @param definitions    the Maxima definitions of this function (one definition per string, each defining
   *                a variable/function; must include the function `name`). */
  case class FunctionDefinition(name: String, dependencies: Seq[FunctionDefinition] = Seq.empty, definitions: Seq[String] = Seq.empty) {
    assert(dependencies.nonEmpty || definitions.nonEmpty)
    /** All definitions needed by this function, including (transitively) those of its helpers. */
    def allDefs: Seq[String] = dependencies.flatMap(_.allDefs) ++ definitions
  }

  /** Represents a Moodle/Stack input field */
  case class Input(typ: InputType,
                   name: String,
                   reference: String,
                   forbidWords: Iterable[String],
                   allowWords: Iterable[String],
                   extraOptions: Iterable[String],
                   insertStars: InsertStars,
                   /** Size of the input field. */
                   boxsize: Int,
                   /** Maxima variable/function definitions needed by this input (e.g. its validator
                    * function). Merged into the question variables at export time. One definition per string. */
                   questionVariables: Seq[String] = Seq.empty,
                  ) {
    assert(name.nonEmpty)
    assert(reference.nonEmpty)
    assert(allowWords.forall(identifierRegex.matches), allowWords)
    assert(extraOptions.forall(optionRegex.matches), extraOptions)
    assert(boxsize > 0)

    def xml: Elem =
      <input>
        <name>{name}</name>
        <type>{typ}</type>
        <tans>{reference}</tans>
        <boxsize>{boxsize}</boxsize>
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
                      questionVariables: Seq[String] = Seq.empty,
                      files: Map[String, Array[Byte]] = Map.empty,
                      inputs: Seq[Input]) {
    def xml: Elem = {
      val filesXML = files map { (name, content) =>
        if (name.contains(' '))
          throw IllegalArgumentException(s"In Moodle export of question ${this.name}, attached file $name contains spaces in file name. Not allowed.")
        val base64 = Base64.getEncoder.encodeToString(content)
        <file name={name} path="/" encoding="base64">
          {base64}
        </file>
      } flatMap {
        Seq(Text("\n"), _)
      }

      val commentField = Input(
        typ = InputType.string,
        name = "COMMENT_FIELD",
        reference = "none",
        boxsize = 25,
        forbidWords = Seq.empty,
        allowWords = Seq.empty,
        extraOptions = Seq.empty,
        insertStars = InsertStars.dontInsert,
      )

      val inputsXML = inputs.appended(commentField).map(_.xml).flatMap {
        Seq(Text("\n"), _)
      }

      // Question-level variables plus the variables contributed by each input (e.g. validator
      // functions and their shared helpers). Identical definitions are deduplicated so that a
      // helper function used by several inputs does not clash; after that, two distinct definitions
      // of the same variable/function name are an error.
      val allDefinitions = (questionVariables ++ inputs.flatMap(_.questionVariables))
        .filter(_.nonEmpty).distinct
      val clashes = allDefinitions.groupBy(definedVariableName).collect {
        case (Some(varName), defs) if defs.size > 1 => varName
      }
      if (clashes.nonEmpty)
        throw IllegalArgumentException(s"In Moodle export of question ${this.name}, these variables are defined more than once: ${clashes.mkString(", ")}")
      // Ensure each definition is terminated (Maxima separates statements with ; or $), otherwise a
      // definition without a trailing terminator would glue onto the next one and cause a syntax error.
      val allQuestionVariables = allDefinitions.map { d =>
        val t = d.stripTrailing
        if (t.endsWith(";") || t.endsWith("$")) t else t + ";"
      }.mkString("\n")

      val footer = s"""\n<hr><p style="color: gray; font-size: smaller; text-align: right;">Optional comments (use sparingly, in case of trouble only):<br>[[input:COMMENT_FIELD]]<br/>\n""" +
        s"""[Question name: ${escapeHtml4(name)}]</p>"""

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
              {scala.xml.PCData(allQuestionVariables)}
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

  lazy val defaultForbiddenWords: Set[String] =
    Utils.allStrings(('a' to 'z') ++ ('A' to 'Z'), 2).toSet

  def inputElementToMoodle(inputElement: InputElement): Input = {
    val name = inputElement.name.toString
    val (allowWords, forbidWords) = inputElement.tags.get(moodleAllowWords) match
      case Some(allow) => (allow, (defaultForbiddenWords -- allow).toSeq.sortBy(_.toLowerCase))
      case None => (Seq.empty, Seq.empty)

    val typ = inputElement.tags(moodleInputType)
    if (Seq(InputType.radio, InputType.checkbox, InputType.dropdown).contains(typ))
      throw IllegalArgumentException(s"Input element $name cannot have tag moodleInputType = $typ")

    // Using ? instead of empty reference since empty reference produces error in Moodle/Stack
    val reference = inputElement.tags.getOrElse(moodleReferenceSolution,
      if (inputElement.reference != "") inputElement.reference else "?")

    // A validator is exported as a `validator:<name>` extra option; its (and its sub-helpers')
    // Maxima definitions are contributed to the question variables. The name must be lowercase:
    // STACK lowercases the identifier when resolving the `validator:` option, so a mixed-case name
    // would not match its definition (leading to "the optional validator threw internal Maxima errors").
    val (validatorOptions, validatorVariables) = inputElement.tags.get(moodleValidation) match {
      case Some(validator) =>
        require(validator.name.matches("[a-z][a-z0-9_]*"),
          s"Validator function name must be lowercase (STACK is case-insensitive): ${validator.name}")
        (Seq(s"validator:${validator.name}"), validator.allDefs)
      case None => (Seq.empty, Seq.empty)
    }

    Input(
      typ = typ,
      name = name,
      reference = reference,
      forbidWords = forbidWords,
      allowWords = allowWords,
      extraOptions = (inputElement.tags(moodleExtraOptions) ++ validatorOptions) appended MoodleExtraOptions.allowEmpty,
      insertStars = inputElement.tags(moodleInsertStars),
      boxsize = inputElement.tags(InputElement.inputElementColumns),
      questionVariables = validatorVariables,
    )
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
      case MultipleChoice.Style.checkbox => InputType.checkbox
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

    val referenceSolution = element.style match {
      case Style.select | Style.radio =>
        val referenceSolutionSeq = element.options.zipWithIndex.map { case ((name, text), index) =>
          val selected = name == element.reference
          s"[${quote(name)}, $selected, ${quote(text)}]"
        }
        "[" + referenceSolutionSeq.mkString(", ") + "]"
      case Style.checkbox =>
        val selected = element.reference == element.yesAnswer
        val label = element.tags(checkboxLabel)
        s"""[["checked", $selected, ${quote(label.html)}]]"""
    }

    Input(
      typ = typ,
      name = name,
      reference = referenceSolution,
      forbidWords = Seq.empty,
      allowWords = Seq.empty,
      //noinspection ScalaDeprecation
      extraOptions = element.tags(moodleExtraOptions) appended MoodleExtraOptions.allowEmpty,
      insertStars = element.tags(moodleInsertStars),
      boxsize = 15, // Not sure if this affects anything.
    )
  }

  def assessmentToQuestion(assessment: Assessment): Question = {
    val inputs = Seq.newBuilder[Input]
    val fileMapBuilder = DefaultFileMapBuilder("@@PLUGINFILE@@/")
    val renderContext = RenderContext(RenderContext.dynamic := false, RenderContext.exam := DummyExam)
    val questionText = assessment.renderHtml { element =>
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
        case _: SolutionElement =>
          Html.empty
        case element: StaticElement =>
          element.renderHtml(renderContext, fileMapBuilder)
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

  /** Specify which symbol names are allowed in Stack math input fields.
   * If not given, the default moodle settings are used (i.e., all 1+2 character symbols are allowed).
   * If given, only the given symbols are allowed (this differs from Moodle's defaults where 1+2 character symbols are allowed unless explicitly forbidden).
   **/
  val moodleAllowWords = Tag[InputElement, Seq[String]]()
  /** A STACK input validator (see [[MoodleValidationHelpers]]). The input gets the extra option
   * `validator:<helper.name>`, and the helper's (and its sub-helpers') Maxima definitions are
   * injected into the question variables (deduplicated across inputs). */
  val moodleValidation = Tag[InputElement, FunctionDefinition]()
  /** Question-level Maxima variable/function definitions (one definition per string). Merged with
   * the definitions contributed by individual input elements (e.g. validators); identical strings
   * are deduplicated, and defining the same name twice is an error. */
  val moodleQuestionVariables = Tag[Assessment, Seq[String]](default = Seq.empty)
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