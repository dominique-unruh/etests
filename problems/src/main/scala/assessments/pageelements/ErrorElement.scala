package assessments.pageelements

import assessments.Comment.Format.html
import assessments.{Assessment, FileMapBuilder, Html, HtmlConvertible}
import org.apache.commons.text.StringEscapeUtils.escapeHtml4

trait ProblemElement extends StaticElement

case class ErrorElement(message: String, files: Map[String, Array[Byte]] = Map.empty) extends ProblemElement {
  override def renderHtml(context: RenderContext, associatedFiles: FileMapBuilder): Html = {
    val fileNames = files.toSeq map { (name, content) => associatedFiles.add(filename = name, mimeType = "text/plain", content = content) }
    val fileLinks = fileNames map { filename => s""" [<a href="${escapeHtml4(filename)}" target="_blank">${escapeHtml4(filename)}</a>]""" }
    Html(s"""<div style="background-color: red">ERROR: ${escapeHtml4(message)}${fileLinks.mkString("")}</div>""")
  }
}

case class TodoElement(message: String | HtmlConvertible) extends ProblemElement {
  override def renderHtml(context: RenderContext, associatedFiles: FileMapBuilder): Html = {
    val html = message match
      case html: HtmlConvertible => html.toHtml
      case text: String => escapeHtml4(text)
    Html(s"""<span class="todo">TODO: $html</span>""")
  }
}
