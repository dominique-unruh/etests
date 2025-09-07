package assessments

import assessments.ElementName.validElementNameRegex

final case class ElementName private[ElementName] (val name: String) {
  override lazy val toString: String = name
  lazy val jsElementCallbackName: String = "element$" + name
  lazy val jsElementId: String = "element-" + name
}

object ElementName {
  private[ElementName] def assertValidElementName(name: String): Unit = {
    assert(validElementNameRegex.matches(name))
  }

  private val validElementNameRegex = "[a-zA-Z][a-zA-Z0-9_]*".r.anchored
  def apply(name: String): ElementName = {
    assertValidElementName(name)
    new ElementName(name)
  }

  val grader: ElementName = ElementName("grader")
  val errordisplay: ElementName = ElementName("errorDisplay")
  val registrationNumber: ElementName = ElementName("registrationNumber")
}