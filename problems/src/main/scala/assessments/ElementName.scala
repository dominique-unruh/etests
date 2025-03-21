package assessments

final case class ElementName private(names: Seq[String]) {
  override lazy val toString: String = names.mkString(".")
  lazy val jsElementCallbackName: String = "element$" + names.mkString("$")
  lazy val jsElementId: String = "element-" + toString
  def dropRight(n: Int): ElementName = {
    val newNames = names.dropRight(n)
    assert(newNames.nonEmpty)
    new ElementName(newNames)
  }
  def last = names.last
}

object ElementName {
  def apply(name: String): ElementName = {
    val names = name.split('.')
    assert(names.nonEmpty)
    assert(names.forall(_.nonEmpty))
    // TODO: assert letters 0-9a-zA-Z_ in names
    new ElementName(names)
  }
}