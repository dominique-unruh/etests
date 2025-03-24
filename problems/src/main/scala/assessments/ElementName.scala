package assessments

class ElementPath protected (val names: Seq[String]) {
  override lazy val toString: String = names.mkString(".")
  def +(name: String) = new ElementPath(names appended name)
  def lastOption: Option[String] = names.lastOption
  //noinspection MutatorLikeMethodIsParameterless
  def removeLast: ElementPath =
    assert(names.nonEmpty)
    new ElementPath(names.dropRight(1))
}

object ElementPath {
  private[assessments] def assertValidElementPath(names: Seq[String]): Unit = {
    assert(names.forall(_.nonEmpty))
    // TODO: assert letters 0-9a-zA-Z_ in names
  }

  def apply(name: String): ElementPath= {
    val names = name.split('.')
    assertValidElementPath(names)
    new ElementPath(names)
  }
  val empty = new ElementPath(Seq.empty)
}

final case class ElementName protected[assessments] (override val names: Seq[String]) extends ElementPath(names) {
  override lazy val toString: String = names.mkString(".")
  lazy val jsElementCallbackName: String = "element$" + names.mkString("$")
  lazy val jsElementId: String = "element-" + toString
  def dropRight(n: Int): ElementName = {
    val newNames = names.dropRight(n)
    assert(newNames.nonEmpty)
    new ElementName(newNames)
  }
  def last: String = names.last
}

object ElementName {
  def apply(name: String): ElementName = {
    val names = name.split('.')
    assert(names.nonEmpty)
    ElementPath.assertValidElementPath(names)
    new ElementName(names)
  }

  def apply(path: ElementPath, name: String): ElementName = {
    val names = name.split('.')
    assert(names.nonEmpty)
    ElementPath.assertValidElementPath(names)
    new ElementName(path.names ++ names)
  }
}