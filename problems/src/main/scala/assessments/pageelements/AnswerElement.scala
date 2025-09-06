package assessments.pageelements

trait AnswerElement extends DynamicElement {
  val reference: String
  def setAction(content: String): Seq[ElementAction]
}
