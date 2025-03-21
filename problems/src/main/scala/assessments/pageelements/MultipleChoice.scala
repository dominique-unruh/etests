package assessments.pageelements

import assessments.{ElementName, Points}

final class MultipleChoice(val name: ElementName,
                           val options: Map[String, String],
                           val correct: String,
                           val points: Points)
  extends PageElement {
  assert(options.contains(correct))
  assert(points > 0)

  override def renderHtml: String = "[NOT IMPLEMENTED: MultipleChoice HTML]"
}