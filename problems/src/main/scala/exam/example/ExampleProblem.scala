package exam.example

import assessments.DynexiteDefaults.*
import assessments.GradingContext.*
import assessments.InterpolatedMarkdown.md
import assessments.pageelements.{Element, InputElement}
import assessments.stack.StackMath
import assessments.{DynexiteDefaults, ExceptionContext, GradingContext, InterpolatedMarkdown, MarkdownAssessment, MathContext, Points}

object ExampleProblem extends MarkdownAssessment {
  override val name = "example problem"
  override val reachablePoints: Points = 8

  lazy val question = md"""
Please enter the number 10, without writing 10.
(E.g. something like \(\sqrt{144}-10\).)

$answer

${preview(answer)}
"""

  val answer: InputElement = input("sqrt(100)")

  override lazy val explanation: InterpolatedMarkdown[Element] = md"""
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
    if (parsed == StackMath.noAnswer)
      return

    if (parsed.toSympyMC() `algebraicEqual` 10)
      comments += "Correct"
      points += reachablePoints
    else
      comments += raw"Doesn't evaluate to 10, but to \(${parsed.toSympyMC().simplify.latex}\)"
  }
}
