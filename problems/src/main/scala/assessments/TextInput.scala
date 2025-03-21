package assessments

final class TextInput(val name: ElementName,
                      val reference: String,
                      val correct: Seq[String] = Nil,
                      val partiallyCorrect: Map[String, Points] = Map.empty,
                      val wrong: Seq[String] = Nil,
                      val points: Points = 1)
  extends PageElement {
  assert(points > 0)

  override def renderHtml: String = "[NOT IMPLEMENTED: MultipleChoice HTML]"
}
