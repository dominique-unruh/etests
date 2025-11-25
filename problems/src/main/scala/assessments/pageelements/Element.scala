package assessments.pageelements

import assessments.*
import com.eed3si9n.eval.Eval
import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.PyQuote
import play.api.libs.json.{JsObject, JsString, JsValue}
import utils.Tag
import utils.Tag.Tags

import scala.util.Using

sealed trait Element

trait StaticElement extends Element {
  def renderHtml(associatedFiles: FileMapBuilder): Html
}

/** Potentially interactive elements on an assessment page. */
trait DynamicElement extends Element { self =>
  val name: ElementName
  /** Human readable name.
   * @return the value of the tag [[DynamicElement.humanName]] or else the name of this element. */
  def humanName: String = tags.getOrElse(DynamicElement.humanName, name.toString)
  def renderHtml: Html
  def renderStaticHtml(answers: Map[ElementName, String]): Html
  def updateAction(assessment: Assessment, state: Map[ElementName, JsValue]): IterableOnce[ElementAction] = Seq.empty
  val tags: Tag.Tags[self.type]
  val initialState: JsValue = JsObject(collection.Seq("content" -> JsString("")))
}

object DynamicElement {
  val humanName = Tag[DynamicElement, String](default = "")
}

case class ElementAction(element: ElementName, data: JsValue)
object ElementAction {
  def error(message: String): ElementAction =
    ElementAction(ElementName.errordisplay, JsObject(Map("message" -> JsString(message))))
}
