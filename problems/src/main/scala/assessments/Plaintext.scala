package assessments

/** Encapsulates plaintext.
 * It is merely a thin wrapper around [[String]] to make it easier to make sure one does not mix up what
 * functions take HTML and what functions plaintext and what functions markdown.
 *
 * @see [[Markdown]], [[Html]]
 **/
final case class Plaintext(text: String) extends HtmlConvertible {
  override def toString: String = super.toString
  override def toHtml: Html = Html.fromPlaintext(text)
}

object Plaintext {
  val empty: Plaintext = Plaintext("")
}