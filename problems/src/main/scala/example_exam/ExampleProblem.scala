package example_exam

import assessments.DynexiteDefaults.*
import assessments.GradingContext.*
import assessments.GradingContext.GraderOutcome.{doesntFire, fires}
import assessments.InterpolatedMarkdown.md
import assessments.pageelements.{Element, InputElement, SolutionElement}
import assessments.math.Math
import assessments.{DynexiteDefaults, ElementName, ExceptionContext, GradingContext, HtmlConvertible, InterpolatedMarkdown, MarkdownAssessment, MathContext, NoGraderYetException, Points}
import com.typesafe.scalalogging.Logger

import scala.util.boundary
import scala.util.boundary.break

object ExampleProblem extends MarkdownAssessment {
  override val name = "Example problem"
  override lazy val reachablePoints: Points = 8

  lazy val question = md"""
Please enter the number 10, without writing 10.
(E.g. something like \(\sqrt{144}-10\).)

$answer

${preview(answer)}

$explanation

$gradingRule1
$gradingRule2

"""


  val answer: InputElement = input("sqrt(100)")

  lazy val explanation = explain(md"""
    For example, \(\sqrt{100}\) would work because it evaluates to 10.
    Of course, there are many other possibilities.
    But 10 itself is not a valid answer.
  """)

  lazy val gradingRule1 = grading(md"""
    * Anything that evaluates to 10 and isn't the string 10 (after trimming whitespace): full points.
  """, reachablePoints, {
    given MathContext = MathContext.default

    if (answer.stringValue.trim == "10")
      doesntFire // handled by gradingRule2
    else {
      val parsed = answer.mathTry
      if (parsed == Math.noAnswer)
        doesntFire
      else if (parsed.toSympyMC() `algebraicEqual` 10) {
        comments += "Correct"
        fires
      } else {
        comments += raw"Doesn't evaluate to 10, but to \(${parsed.toSympyMC().simplify.latex}\)"
        doesntFire
      }
    }
  })

  lazy val gradingRule2 = grading(md"""The number 10: half points.""", reachablePoints / 2, {
    if (answer.stringValue.trim == "10") {
      comments += "You entered 10 literally. Half points"
      fires
    } else doesntFire
  }, partial = true)

  private val logger = Logger[ExampleProblem.type]
}

