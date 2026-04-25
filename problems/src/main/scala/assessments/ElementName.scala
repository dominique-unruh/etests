package assessments

import assessments.ElementName.validElementNameRegex
import org.apache.commons.text.StringEscapeUtils

final case class ElementName private[ElementName] (val name: String) {
  override lazy val toString: String = name
  lazy val jsElementCallbackName: String = "element$" + name
  lazy val jsElementId: String = "element-" + name
  def htmlComponentName: String = "etest-" + name
  def htmlComponentNameEscaped: String = StringEscapeUtils.escapeHtml4(htmlComponentName)
}

object ElementName {
  private[ElementName] def assertValidElementName(name: String): Unit = {
    assert(validElementNameRegex.matches(name))
  }

  private val validElementNameRegex = "(__)?[a-zA-Z][a-zA-Z0-9_-]*".r.anchored
  def apply(name: String): ElementName = {
    assertValidElementName(name)
    new ElementName(name)
  }

  def fromHtmlComponentName(name: String): ElementName = {
    assert(name.startsWith("etest-"))
    ElementName(name.stripPrefix("etest-"))
  }
  
  val grader: ElementName = ElementName("grader")
  val errordisplay: ElementName = ElementName("errorDisplay")
  val registrationNumber: ElementName = ElementName("registration-number")
  /** For any extra data that should be displayed */
  val extraData: ElementName = ElementName("__extraData")
}