package assessments.pageelements

import assessments.{GradingContext, Points}
import utils.{Tag, Utils}

trait AnswerElement extends DynamicElement {
  /** Reference solution */
  val reference: String
  def setAction(content: String): Seq[ElementAction]

  /**
   * Performs simple grading of this answer element against its reference value.
   *
   * This method compares the student's answer (retrieved from the grading context)
   * with the reference answer using a simple string equality check (ignoring leading/trailing whitespace).
   * It updates the grading context with feedback and points based on
   * the comparison result.
   *
   * Grading logic:
   * - Correct answer: Awards full points and adds success message
   * - Empty answer: Awards 0 points and adds "not answered" message
   * - Incorrect answer: Awards 0 points and adds "incorrect" message
   *
   * @param points         The number of points to award for a correct answer
   */
  def simpleGrade(points: Points)(using gradingContext: GradingContext): Unit = {
    val answer = gradingContext.answers(this.name)
    val correct = answer.trim == reference.trim // We could configure alternative tests
    if (correct)
      gradingContext += s"${humanName.capitalize}: correct. $points points."
      gradingContext.points += points
    else if (answer == "")
      gradingContext += s"${humanName.capitalize}: not answered. 0 points / $points points."
    else
      gradingContext += s"${humanName.capitalize}: incorrect. 0 points / $points points."
  }
}

object AnswerElement {
}