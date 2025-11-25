package exam.example

import assessments.DynexiteDefaults.*
import assessments.GradingContext.*
import assessments.InterpolatedMarkdown.md
import assessments.pageelements.{Element, InputElement}
import assessments.stack.StackMath
import assessments.{DynexiteDefaults, ExceptionContext, GradingContext, InterpolatedMarkdown, MarkdownAssessment, MathContext, Points}

object ExampleProblem2 extends MarkdownAssessment {
  override val name = "Example problem 2"
  override val reachablePoints: Points = 8

  lazy val question = md"""
Please enter the number 10 any way you like.

$answer

${preview(answer)}
"""

  val answer: InputElement = input("10")

  override lazy val explanation: InterpolatedMarkdown[Element] = md"""
For example, 10 would work.
Of course, there are many other possibilities like \(5+5\).
      """

  override lazy val gradingRules: InterpolatedMarkdown[Element] = md"""
* Anything that evaluates to 10: full points.
      """

  override def grade()(using context: GradingContext, exceptionContext: ExceptionContext): Unit = {
    given MathContext = MathContext.default

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
