package assessments

abstract class Grader(val name: ElementName) extends PageElement {
  override def renderHtml: String = ""
  def grade(answers: Map[ElementName, String]): (Points, Seq[String])
  val points: Points
}
