package utils

import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer

object Markdown {
  private val markdownParser = Parser.builder.build
  private val markdownRenderer = HtmlRenderer.builder.build

  def markdownToHtml(markdown: String): String = {
    val markdownEscaped = markdown.replace("\\", "\\\\") // commonmark parse treats \( as ( etc. So we quote the \. Could be refinded
    val parsed = markdownParser.parse(markdownEscaped)
    markdownRenderer.render(parsed)
  }
}
