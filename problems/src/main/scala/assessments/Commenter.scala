package assessments

import assessments.Comment.Format.markdown
import assessments.Comment.Kind.{feedback, warning}
import org.apache.commons.text.StringEscapeUtils
import utils.Markdown

import scala.annotation.targetName
import scala.collection.mutable

final class Commenter {
  var points: Points = 0 
  private val builder = Seq.newBuilder[Comment]
  @targetName("addString")
  def +=(comment: String): Unit = builder += Comment(text = comment, format = markdown, kind = feedback)
  @targetName("addComment")
  def +=(comment: Comment): Unit = builder += comment
  def comments: Seq[Comment] = builder.result()
  def clear(): Unit = builder.clear()
}

case class Comment(val html: String, val kind: Comment.Kind)

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

