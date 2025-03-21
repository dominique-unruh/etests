package assessments

import assessments.pageelements.PageElement

abstract class Grader(val name: ElementName) extends PageElement {
  override def renderHtml: String = ""
  def grade(answers: Map[ElementName, String]): (Points, Seq[String])
  lazy val points: Points
}
