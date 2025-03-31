package assessments

import assessments.pageelements.PageElement

abstract class Grader(val name: ElementName) extends PageElement {
  override def renderHtml: String = ""
  def grade(gradingContext: GradingContext): (Points, Seq[String])
  lazy val points: Points
}
