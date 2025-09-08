package assessments

import assessments.GradingContext.comments

import scala.language.implicitConversions
import assessments.pageelements.*
import assessments.pageelements.DynamicElement.humanName
import assessments.pageelements.MultipleChoice.Style.select
import assessments.stack.StackParser.parse
import assessments.stack.StackUtils.checkEquality
import assessments.stack.{StackMath, SympyAssumption, SympyExpr}
import utils.Tag.Tags
import utils.Utils

import java.io.IOException
import scala.collection.SeqMap

object DynexiteDefaults {
//  given sympyCache: Cache[SympyExpr] = CaffeineCache[SympyExpr]

  private def elementName(name: sourcecode.Name) =
    ElementName(name.value.replace('$', '.'))

  def input(reference: String, tags: Tags[InputElement] = Tags.empty)(using name: sourcecode.Name): InputElement =
    new InputElement(elementName(name), reference, tags)

  def multi(options: Seq[String | (String,String)], reference: String,
           tags: Tags[MultipleChoice] = Tags.empty, style: MultipleChoice.Style = select)(using name: sourcecode.Name): MultipleChoice = {
    val optionMap = SeqMap.from(options.map { case option: String => option -> option; case (option,text) => option -> text})
    new MultipleChoice(name=elementName(name), options=optionMap, reference=reference, style=style, tags=tags)
  }

  extension (str: String) {
    @deprecated("Use .math")
    def sympy: SympyExpr = {
      if (str == "" || str == null)
        SympyExpr.errorTerm("empty")
      else try
//        caching("sympySource", str)(None) { parse(str).toSympy }
        parse(str).toSympy
      catch
        case e: SyntaxError => SympyExpr.errorTerm(e.getMessage)
    }
    def math(inputElement: InputElement): StackMath =
      parse(str, inputElement)
    /**
     * Attempts to parse and evaluate a mathematical expression from this string
     * with error handling for syntax errors.
     * 
     * (E.g., `"1+2".mathTry("first question", ans1)`)
     *
     * This method tries to parse the mathematical content (with the parser of `inputElement`)
     * If the input is empty or contains syntax errors, it returns a "no answer" result ([[StackMath.noAnswer]])
     * and logs appropriate feedback to the grading context.
     *
     * @param name           The human-readable name of the input field, used for error messages
     * @param inputElement   The input element relative to which parsing should take place
     * @return [[StackMath.noAnswer]] if input is empty or contains syntax errors,
     *         otherwise returns the parsed mathematical result from math(inputElement)
     */
    def mathTry(name: String, inputElement: InputElement)(using gradingContext: GradingContext): StackMath = {
      if (str == "")
        StackMath.noAnswer
      else
        try
          math(inputElement)
        catch
          case e: SyntaxError =>
            comments += s"Could not parse $name (error: ${e.getMessage}), treating as no answer"
            StackMath.noAnswer
    }
  }

  // Not using "extension (pe: PageElement) because that exports additionally methods DynexiteDefault.latex... that may conflict with equally named methods when DynexiteDefaults.* is imported
  implicit class PageElementMethods(pe: DynamicElement) {
    def stringValue(using gradingContext: GradingContext): String = GradingContext.answers.getOrElse(pe.name, "")
  }

  implicit class InputElementMethods(ie: InputElement) {
    @deprecated("Use .math.toSympyMC()")
    def sympy(using gradingContext: GradingContext): SympyExpr = ie.stringValue.sympy
    def latex(using gradingContext: GradingContext, mathContext: MathContext): String = math.toSympyMC(allowUndefined = true).latex
    def math(using gradingContext: GradingContext): StackMath = ie.stringValue.math(ie)
    def refmath: StackMath = ie.reference.math(ie)
    def mathTry(using gradingContext: GradingContext): StackMath =
      ie.stringValue.mathTry(ie.humanName, ie)
  }

/*  extension (pe: PageElement) {
    def stringValue(using gradingContext: GradingContext): String = gradingContext.answers.getOrElse(pe.name, "")
    def sympy(using gradingContext: GradingContext): SympyExpr = stringValue.sympy
    def latex(using gradingContext: GradingContext): String = sympy.latex
  }*/

  private val renderMathContext: MathContext = MathContext.default
    .fixVar("i", StackMath.imaginaryUnit)
    .fixVar("e", StackMath.eulerConstant)
    .fixVar("pi", StackMath.pi)
  
  private def stackMathRender(pageElement: InputElement)(string: String): String = {
    given MathContext = renderMathContext
    if (string == "")
      ""
    else
      string.math(pageElement).toSympyMC(allowUndefined = true, allowUndefinedFunctions = true).latex
  }

  def preview(observed: InputElement)(using name: sourcecode.Name): MathPreviewElement = {
    val name2 = if (name.value == "question" || name.value == "explanation") // Inlined in the markdown, not a good default
      ElementName(s"${observed.name}_preview")
    else
      ElementName(name.value)
    MathPreviewElement(name2, observed.name, stackMathRender(observed))
  }

  /** Checks for equality of two Sympy expressions (`x==y`?)
   * Up to mathematical equivalence, as far as can be figured out (somewhat heuristic).
   *
   * @param x Either a sympy expression, or an answer field
   *          (in which case the sympy expression will automatically be retrieved).
   * @param y Analogous to `x`
   * @param assumption Assumption to pass to Sympy (e.g., all variables are positive).
   */
  @deprecated
  def checkEq(x: => InputElement | SympyExpr,
              y: => InputElement | SympyExpr,
              assumption: SympyAssumption = SympyAssumption.positive)
             (using context: GradingContext): Boolean =
    try {
      def toSympy(value: InputElement | SympyExpr) = value match {
        case x: InputElement => x.sympy
        case x: SympyExpr => x
      }
      checkEquality(toSympy(x), toSympy(y), assumption=assumption)
    } catch
      case e : SyntaxError =>
        comments += e.getMessage; false
  
  def gradeInputGroup(inputs: Seq[(AnswerElement, String)],
                      pointsPerOption: Points = null, pointsTotal: Points = null)
                     (using context: GradingContext): Unit = {
    assert(inputs.nonEmpty)
    assert(pointsPerOption != null || pointsTotal != null)
    if (pointsPerOption != null && pointsTotal != null)
      assert(pointsTotal == pointsPerOption * inputs.length, (pointsTotal, pointsPerOption))
    val pointsPerOption2 = if (pointsPerOption==null) pointsTotal / inputs.length else pointsPerOption
    val pointsTotal2 = if (pointsTotal==null) pointsPerOption * inputs.length else pointsTotal

    var points: Points = 0

    for ((input, description) <- inputs) {
      val stringValue = input.stringValue 
      if (stringValue == input.reference)
        assert(stringValue == "" || input.asInstanceOf[MultipleChoice].options.contains(stringValue))
        comments += s"$description: Correct."
        points += pointsPerOption2
      else if (stringValue == "")
        comments += s"$description: Incorrect. (You selected nothing, should be '${input.reference}')"
      else
        comments += s"$description: Incorrect. (You said '${stringValue}', should be '${input.reference}')"
    }

    GradingContext.points += points
  }
}
