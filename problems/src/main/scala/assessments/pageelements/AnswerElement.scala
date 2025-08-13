package assessments.pageelements

trait AnswerElement extends PageElement {
  val reference: String
  def setAction(content: String): Seq[ElementAction]
}
