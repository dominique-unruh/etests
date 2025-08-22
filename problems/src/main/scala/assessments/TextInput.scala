package assessments

import assessments.pageelements.{ElementAction, PageElement}
import play.api.libs.json.JsValue
import utils.Tag.Tags

// TODO what is this? Used?
final class TextInput(val name: ElementName,
                      val reference: String,
                      val correct: Seq[String] = Nil,
                      val partiallyCorrect: Map[String, Points] = Map.empty,
                      val wrong: Seq[String] = Nil,
                      val points: Points = 1,
                      override val tags: Tags[TextInput])
  extends PageElement {
  assert(points > 0)

  override def renderHtml: Html = Html("[NOT IMPLEMENTED: MultipleChoice HTML]")
  override def renderStaticHtml(answers: Map[ElementName, String]): Html = renderHtml
}
