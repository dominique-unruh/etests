package assessments.pageelements

import assessments.{Assessment, FileMapBuilder, Html}
import org.apache.commons.text.StringEscapeUtils.escapeHtml4

case class ErrorElement(message: String, files: Map[String, Array[Byte]]) extends StaticElement {
  override def renderHtml(associatedFiles: FileMapBuilder): Html = {
    val fileNames = files.toSeq map { (name, content) => associatedFiles.add(filename = name, mimeType = "text/plain", content = content) }
    val fileLinks = fileNames map { filename => s""" [<a href="${escapeHtml4(filename)}" target="_blank">${escapeHtml4(filename)}</a>]""" }
    Html(s"""<div style="background-color: red">ERROR: ${escapeHtml4(message)}${fileLinks}</div>""")
  }
}