package assessments

import assessments.Comment.{Kind, seqToHtml, seqToPlaintext}
import assessments.Comment.Kind.feedback
import org.apache.commons.text.StringEscapeUtils
import org.jsoup.Jsoup
import utils.{IndentedInterpolator, Markdown}

import java.awt.color.CMMException

sealed trait Comment {
  val kind: Comment.Kind
  def toHtml: Html
  def toHtmlColored: Html = kind match
    case Kind.feedback => toHtml
    case Kind.debug => Html(s"""<span style="color:gray">${toHtml.html}</span>""")

  def toPlaintext: String = {
    val html = toHtml.html
    val text = try Jsoup.parse(html).text() catch case _ => html.trim
    text
  }

  def filterFeedback: Option[Comment] = kind match
    case Kind.feedback => Some(this)
    case Kind.debug => None
}

case class HtmlComment(html: Html, kind: Comment.Kind) extends Comment {
  override def toHtml: Html = html
}
case class PlaintextComment(text: String, kind: Comment.Kind) extends Comment {
  override def toPlaintext: String = text
  override def toHtml: Html = Html.fromPlaintext(text)
}
case class MarkdownComment(markdown: Markdown, kind: Comment.Kind) extends Comment {
  override def toPlaintext: String = markdown.markdown
  override def toHtml: Html = markdown.toHtml
}
case class NestedComment(comments: Seq[Comment], kind: Comment.Kind) extends Comment {
  override def toHtml: Html = seqToHtml(comments)
  override def toPlaintext: String = seqToPlaintext(comments)
  override def filterFeedback: Option[Comment] = kind match
    case Kind.feedback => Some(copy(comments = Comment.filterFeedback(comments)))
    case Kind.debug => None
}

object Comment {
  enum Kind {
    case feedback, debug
  }
  enum Format {
    case html, markdown, plain
  }
  def apply(text: String, kind: Kind, format: Format): Comment =
    format match
      case Format.html => HtmlComment(Html(text), kind)
      case Format.markdown => MarkdownComment(assessments.Markdown(text), kind)
      case Format.plain => PlaintextComment(text, kind)

//  def warning(markdown: String): Comment = Comment(markdown, kind = Kind.warning, format = Format.markdown)
  def debug(markdown: String): Comment = Comment(markdown, kind = Kind.debug, format = Format.markdown)

  def seqToHtml(comments: Seq[Comment]): Html = {
    val result = new StringBuilder
    result ++= "<ul>\n"
    for (comment <- comments) {
      result ++= s"  <li>${comment.toHtmlColored}</li>\n"
    }
    result ++= "</ul>\n"
    Html(result.result())
  }

  def seqToPlaintext(comments: Seq[Comment]): String = {
    val result = new StringBuilder
    for (comment <- comments) {
      result ++= ind"* ${comment.toPlaintext}"
    }
    result.result()
  }

  def filterFeedback(comments: Seq[Comment]): Seq[Comment] =
    for (comment <- comments;
         comment2 <- comment.filterFeedback)
      yield comment
}
