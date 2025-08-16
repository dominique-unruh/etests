package assessments

import assessments.pageelements.*
import assessments.pageelements.MultipleChoice.Style.select
import assessments.stack.StackParser.parse
import assessments.stack.StackUtils.checkEquality
import assessments.stack.{StackMath, SympyAssumption, SympyExpr}
import utils.Tag.Tags
import utils.Utils

import scala.collection.SeqMap

object DynexiteDefaults {
//  given sympyCache: Cache[SympyExpr] = CaffeineCache[SympyExpr]

  private def elementName(name: sourcecode.Name) =
    ElementName(name.value.replace('$', '.'))

  def input(reference: String, tags: Tags[InputElement] = Tags.empty)(using name: sourcecode.Name): InputElement =
    new InputElement(elementName(name), reference, tags)

  def multi(options: Seq[String | (String,String)], reference: String, style: MultipleChoice.Style = select)(using name: sourcecode.Name): MultipleChoice = {
    val optionMap = SeqMap.from(options.map { case option: String => option -> option; case (option,text) => option -> text})
    new MultipleChoice(name=elementName(name), options=optionMap, reference=reference, style=style)
  }

  extension (str: String) {
    def sympy: SympyExpr = {
      if (str == "" || str == null)
        SympyExpr.errorTerm("empty")
      else try
//        caching("sympySource", str)(None) { parse(str).toSympy }
        parse(str).toSympy
      catch
        case e: SyntaxError => SympyExpr.errorTerm(e.getMessage)
    }
    def math: StackMath =
      parse(str)
  }

  // Not using "extension (pe: PageElement) because that exports additionally methods DynexiteDefault.latex... that may conflict with equally named methods when DynexiteDefaults.* is imported
  implicit class PageElementMethods(pe: PageElement) {
    def stringValue(using gradingContext: GradingContext): String = gradingContext.answers.getOrElse(pe.name, "")
    def sympy(using gradingContext: GradingContext): SympyExpr = stringValue.sympy
    def latex(using gradingContext: GradingContext): String = sympy.latex
    def math(using gradingContext: GradingContext): StackMath = stringValue.math
  }

/*  extension (pe: PageElement) {
    def stringValue(using gradingContext: GradingContext): String = gradingContext.answers.getOrElse(pe.name, "")
    def sympy(using gradingContext: GradingContext): SympyExpr = stringValue.sympy
    def latex(using gradingContext: GradingContext): String = sympy.latex
  }*/

  private def stackMathRender(string: String): String =
    if (string == "")
      ""
    else try
      string.sympy.latex
    catch
      case e: Exception =>
        s"\\text{ERROR: ${Utils.escapeTeX(e.toString)}}"

  def preview(observed: PageElement)(using name: sourcecode.Name): MathPreviewElement = {
    val name2 = if (name.value == "question" || name.value == "explanation") // Inlined in the markdown, not a good default
      ElementName(observed.name, "preview")
    else
      ElementName(name.value)
    MathPreviewElement(name2, observed.name, stackMathRender)
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
  def checkEq(x: => PageElement | SympyExpr,
              y: => PageElement | SympyExpr,
              assumption: SympyAssumption = SympyAssumption.positive)
             (using gradingContext: GradingContext, comments: Commenter): Boolean =
    try {
      def toSympy(value: PageElement | SympyExpr) = value match {
        case x: PageElement => x.sympy
        case x: SympyExpr => x
      }
      checkEquality(toSympy(x), toSympy(y), assumption=assumption)
    } catch
      case e : SyntaxError => comments += e.getMessage; false
  
  def gradeInputGroup(inputs: Seq[(AnswerElement, String)],
                      pointsPerOption: Points = null, pointsTotal: Points = null,
                      commenter: Commenter)
                     (using gradingContext: GradingContext): Unit = {
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
        commenter += s"$description: Correct."
        points += pointsPerOption2
      else if (stringValue == "")
        commenter += s"$description: Incorrect. (You selected nothing, should be ${input.reference})"
      else
        commenter += s"$description: Incorrect. (You said '${stringValue}', should be ${input.reference})"
    }

    commenter.points += points
  }
}
