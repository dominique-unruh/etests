package assessments.pageelements

trait AnswerElement[T] extends PageElement {
  val reference: T
}
