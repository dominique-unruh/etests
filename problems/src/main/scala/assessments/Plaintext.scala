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

  /** Render this plaintext as [[Markdown]] that displays verbatim, i.e. every ASCII punctuation
   * character is backslash-escaped (CommonMark allows escaping any of them). */
  def toMarkdown: Markdown = Markdown(text.flatMap { c =>
    if (Plaintext.asciiPunctuation.contains(c)) s"\\$c" else c.toString
  })
}

object Plaintext {
  val empty: Plaintext = Plaintext("")
  /** ASCII punctuation, all of which may be backslash-escaped in CommonMark. */
  private val asciiPunctuation: Set[Char] = """!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~""".toSet
}