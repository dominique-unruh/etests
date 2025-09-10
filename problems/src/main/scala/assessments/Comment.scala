package assessments

import assessments.Comment.{Kind, seqToHtml, seqToPlaintext, wrapLi}
import assessments.Comment.Kind.feedback
import org.apache.commons.text.StringEscapeUtils
import org.jsoup.Jsoup
import utils.{IndentedInterpolator, Markdown}

import java.awt.color.CMMException

sealed trait Comment {
  val kind: Comment.Kind
  /** @param withLi If true, the html will be a `<li>` or `<ul>` */
  def toHtml(withLi: Boolean): Html
  def toHtmlColored(withLi: Boolean): Html = kind match
    case Kind.feedback => toHtml(withLi)
    case Kind.debug => Html(s"""<span style="color:gray">${toHtml(withLi).html}</span>""")

  def toPlaintext: String = {
    val html = toHtml(withLi=false).html
    val text = try Jsoup.parse(html).text() catch case _ => html.trim
    text
  }

  def filterFeedback: Option[Comment] = kind match
    case Kind.feedback => Some(this)
    case Kind.debug => None
}

case class HtmlComment(html: Html, kind: Comment.Kind) extends Comment {
  override def toHtml(withLi: Boolean): Html = wrapLi(html, withLi)
}
case class PlaintextComment(text: String, kind: Comment.Kind) extends Comment {
  override def toPlaintext: String = text
  override def toHtml(withLi: Boolean): Html = wrapLi(Html.fromPlaintext(text), withLi)
}
case class MarkdownComment(markdown: Markdown, kind: Comment.Kind) extends Comment {
  override def toPlaintext: String = markdown.markdown
  override def toHtml(withLi: Boolean): Html = wrapLi(markdown.toHtml, withLi)
}
case class NestedComment(comments: Seq[Comment], kind: Comment.Kind) extends Comment {
  override def toHtml(withLi: Boolean): Html = seqToHtml(comments)
  override def toPlaintext: String = seqToPlaintext(comments)
  override def filterFeedback: Option[Comment] = kind match
    case Kind.feedback => {
      val filtered = Comment.filterFeedback(comments)
      if (filtered.isEmpty)
        None
      else
        Some(copy(comments = filtered))
    }
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
  def feedback(markdown: String): Comment = Comment(markdown, kind = Kind.feedback, format = Format.markdown)

  given Conversion[String, Comment] = feedback
  
  def seqToHtml(comments: Seq[Comment]): Html = {
    val result = new StringBuilder
    result ++= "<ul>\n"
    for (comment <- comments) {
      result ++= ind"  ${comment.toHtmlColored(withLi=true).html}\n"
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
      yield comment2

  protected [assessments] def wrapLi(html: Html, withLi: Boolean): Html = {
    if (withLi)
      Html(ind"<li>${html.html}</li>")
    else
      html
  }
}
