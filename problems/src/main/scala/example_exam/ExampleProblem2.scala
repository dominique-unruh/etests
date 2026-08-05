package example_exam

import assessments.DynexiteDefaults.*
import assessments.GradingContext.*
import assessments.InterpolatedMarkdown.md
import assessments.pageelements.{Element, InputElement, StaticElement}
import assessments.math.Math
import assessments.{Common, DynexiteDefaults, ExceptionContext, GradingContext, InterpolatedMarkdown, MarkdownAssessment, MathContext, Points}

object ExampleProblem2 extends MarkdownAssessment {
  override val name = "Example problem 2"
  override lazy val reachablePoints: Points = 8

  lazy val someImage: StaticElement = Common.latex("$\\sqrt{123}$")

  lazy val question = md"""
Please enter the number 10 any way you like.

$answer

${preview(answer)}

And an arbitary picture:
$someImage

$explanation

$gradingRules
"""

  val answer: InputElement = input("10")

  lazy val explanation = explain(md"""
      For example, 10 would work.
      Of course, there are many other possibilities like \(5+5\).
  """)

  lazy val gradingRules = grading(md"""
    * Anything that evaluates to 10: full points.
  """) {
    given MathContext = MathContext.default

    val parsed = answer.mathTry
    if (parsed == Math.noAnswer)
      done()

    if (parsed.toSympyMC() `algebraicEqual` 10)
      comments += "Correct"
      points += reachablePoints
    else
      comments += raw"Doesn't evaluate to 10, but to \(${parsed.toSympyMC().simplify.latex}\)"
  }
}
