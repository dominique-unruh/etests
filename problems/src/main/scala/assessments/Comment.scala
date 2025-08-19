package assessments

import assessments.Comment.Kind
import org.apache.commons.text.StringEscapeUtils
import org.jsoup.Jsoup
import utils.Markdown

case class Comment(val html: String, val kind: Comment.Kind) {
  def toPlaintext: String = {
    val text = try Jsoup.parse(html).text() catch case _ => html.trim
    kind match
      case Kind.feedback => text
      case Kind.warning => s"WARNING: $text"
      case Kind.debug => s"debug: $text"
  }
}

object Comment {
  enum Kind {
    case feedback, warning, debug
  }
  enum Format {
    case html, markdown, plain
  }
  def apply(text: String, kind: Kind, format: Format): Comment = {
    val html = format match
      case Format.html => text
      case Format.markdown => Markdown.markdownToHtml(text)
      case Format.plain => StringEscapeUtils.escapeHtml4(text)
    new Comment(html = html, kind = kind)
  }
  def warning(markdown: String): Comment = Comment(markdown, kind = Kind.warning, format = Format.markdown)
  def debug(markdown: String): Comment = Comment(markdown, kind = Kind.debug, format = Format.markdown)
}
