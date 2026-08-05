package example_exam

import assessments.DynexiteDefaults.*
import assessments.GradingContext.*
import assessments.InterpolatedMarkdown.md
import assessments.pageelements.{Element, InputElement, SolutionElement}
import assessments.math.Math
import assessments.{DynexiteDefaults, ElementName, ExceptionContext, GradingContext, HtmlConvertible, InterpolatedMarkdown, MarkdownAssessment, MathContext, Points}

object ExampleProblem extends MarkdownAssessment {
  override val name = "Example problem"
  override lazy val reachablePoints: Points = 8

  lazy val question = md"""
Please enter the number 10, without writing 10.
(E.g. something like \(\sqrt{144}-10\).)

$answer

${preview(answer)}

$xxx
"""

  def expl(text: InterpolatedMarkdown[HtmlConvertible])(using name: sourcecode.Name): SolutionElement = {
    if (name.value == "question" || name.value == "explanation") // Inlined in the markdown, not a good default
      throw RuntimeException("expl called inside question markdown. Put into own val.")
    val name2 = ElementName(name.value)
    SolutionElement(name2, text)
  }


  def grading(text: InterpolatedMarkdown[HtmlConvertible])(using name: sourcecode.Name): SolutionElement = {
    if (name.value == "question" || name.value == "explanation") // Inlined in the markdown, not a good default
      throw RuntimeException("grading called inside question markdown. Put into own val.")
    val name2 = ElementName(name.value)
    SolutionElement(name2, text)
  }

  lazy val xxx = expl(md"""Hello **there**.""")

  val answer: InputElement = input("sqrt(100)")

  override lazy val explanation: InterpolatedMarkdown[HtmlConvertible] = md"""
For example, \(\sqrt{100}\) would work because it evaluates to 10.
Of course, there are many other possibilities.
But 10 itself is not a valid answer.
      """

  override lazy val gradingRules: InterpolatedMarkdown[Element] = md"""
* Anything that evaluates to 10 and isn't the string 10 (after trimming whitespace): full points.
* The number 10: half points.
      """

  override def grade()(using context: GradingContext, exceptionContext: ExceptionContext): Unit = {
    given MathContext = MathContext.default

    if (answer.stringValue.trim == "10")
      comments += "You entered 10 literally. Half points"
      points += reachablePoints / 2
      return

    val parsed = answer.mathTry
    if (parsed == Math.noAnswer)
      return

    if (parsed.toSympyMC() `algebraicEqual` 10)
      comments += "Correct"
      points += reachablePoints
    else
      comments += raw"Doesn't evaluate to 10, but to \(${parsed.toSympyMC().simplify.latex}\)"
  }
}
