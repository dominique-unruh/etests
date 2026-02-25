package assessments

/** Encapsulates plaintext.
 * It is merely a thin wrapper around [[String]] to make it easier to make sure one does not mix up what
 * functions take HTML and what functions plaintext and what functions markdown.
 *
 * @see [[Markdown]], [[Html]]
 **/
final case class Plaintext(html: String) extends AnyVal {
  override def toString: String = super.toString
}

object Plaintext {
  val empty: Plaintext = Plaintext("")
}