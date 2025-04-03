package assessments.pageelements

import assessments.{ElementName, Points}

final class MultipleChoice(val name: ElementName,
                           val options: Seq[String],
                           val reference: String)
  extends AnswerElement {
  assert(options.contains(reference), (options, reference))

  override def renderHtml: String = "[NOT IMPLEMENTED: MultipleChoice HTML]"
}