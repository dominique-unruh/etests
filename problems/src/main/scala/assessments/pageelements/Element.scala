package assessments.pageelements

import assessments.*
import com.eed3si9n.eval.Eval
import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.PyQuote
import play.api.libs.json.{JsObject, JsString, JsValue}
import utils.Tag
import utils.Tag.Tags

import scala.concurrent.Future
import scala.util.Using

sealed trait Element {
  /** Renders this element to an HTML fragment for the assessment page.
   *
   * Called by [[Assessment.renderHtml]] while assembling the full page. The `context` selects the
   * rendering mode — e.g. `RenderContext.dynamic` picks an interactive input widget versus a static
   * snapshot, and carries whose answers to display. Any binary assets the fragment references
   * (images, generated files) are registered via `associatedFiles`, which returns the filename to
   * link to; the collected files are emitted alongside the HTML.
   *
   * @param context rendering mode and answer-display settings
   * @param associatedFiles sink for binary assets; [[FileMapBuilder.add]] returns their referenceable name
   * @return the HTML fragment for this element */
  def renderHtml(context: RenderContext, associatedFiles: FileMapBuilder): Html
}

trait StaticElement extends Element

/** Potentially interactive elements on an assessment page. */
trait DynamicElement extends Element { self =>
  val name: ElementName
  /** Human readable name.
   * @return the value of the tag [[DynamicElement.humanName]] or else the name of this element. */
  def humanName: String = tags.getOrElse(DynamicElement.humanName, name.toString)
  /** Live feedback for this element, recomputed whenever the user changes their answer.
   *
   * The web app POSTs the current answer state as the user edits the page (see the frontend
   * `state-manager.ts` and `AssessmentController.getFeedback`); [[Assessment.getFeedback]] then
   * calls this method on every [[DynamicElement]] and ships the returned JSON back to the client to
   * update that element's display. Runs asynchronously and is cached per `(assessment, name, state)`;
   * if it does not complete within the feedback timeout, [[timeoutFeedback]] supplies a fallback.
   *
   * Implementations vary: a [[assessments.Grader]] returns live grading, [[MathPreviewElement]]
   * returns a rendered math preview, while plain input elements return `JsNull` (no feedback).
   *
   * @param state the current answers, keyed by [[ElementName]]
   * @return the feedback payload for this element, consumed by the frontend renderer */
  def getFeedback(assessment: Assessment, state: Map[ElementName, JsValue]): Future[JsValue]
  /** Fallback feedback used when [[getFeedback]] does not finish within the feedback timeout.
   *
   * [[Assessment.getFeedback]] awaits all elements' [[getFeedback]] futures with a deadline; for any
   * that time out it calls this method instead and marks the batch as timed out. Must return
   * synchronously and cheaply (no blocking computation).
   *
   * @param state the current answers, keyed by [[ElementName]]
   * @return the placeholder feedback payload shown until a full result is available */
  def timeoutFeedback(assessment: Assessment, state: Map[ElementName, JsValue]): JsValue
  val tags: Tag.Tags[self.type]
//  val initialState: JsValue = JsObject(collection.Seq("content" -> JsString("")))
}

object DynamicElement {
  val humanName = Tag[DynamicElement, String](default = "")
  val hourglass = JsString(s"""<span style="color:gray; font-weight:bold;">⌛</span>""")
}

case class ElementAction(element: ElementName, data: JsValue)
object ElementAction {
  def error(message: String): ElementAction =
    ElementAction(ElementName.errordisplay, JsObject(Map("message" -> JsString(message))))
  def extraData(data: String): ElementAction =
    ElementAction(ElementName.extraData, JsObject(Map("data" -> JsString(data))))
}
