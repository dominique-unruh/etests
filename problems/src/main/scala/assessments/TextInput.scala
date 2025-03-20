package assessments

final class TextInput(val name: ElementName,
                      val correct: Seq[String],
                      val partiallyCorrect: Map[String, Points],
                      val wrong: Seq[String],
                      val points: Points)
  extends PageElement {
  assert(points > 0)

  override def renderHtml: String = "[NOT IMPLEMENTED: MultipleChoice HTML]"
}
