package assessments.pageelements

import assessments.{ElementName, Points}
import utils.Tag
import utils.Tag.Tags

final class MultipleChoice(val name: ElementName,
                           val options: Seq[String],
                           val reference: String)
  extends AnswerElement {
  override val tags: Tag.Tags[MultipleChoice.this.type] = Tags.empty
  assert(options.contains(reference), (options, reference))

  override def renderHtml: String = "[NOT IMPLEMENTED: MultipleChoice HTML]"

  override def setAction(content: String): Seq[ElementAction] = ???
}