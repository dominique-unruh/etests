package utils

import org.commonmark.ext.gfm.tables.TablesExtension
import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer

import scala.jdk.CollectionConverters.SeqHasAsJava

object Markdown {
  private val extensions = Seq(TablesExtension.create()).asJava
  private val markdownParser = Parser.builder
    .extensions(extensions)
    .build
  private val markdownRenderer = HtmlRenderer.builder
    .extensions(extensions)
    .build
  
  def markdownToHtml(markdown: String): String = {
    val markdownEscaped = markdown.replace("\\", "\\\\") // commonmark parse treats \( as ( etc. So we quote the \. Could be refinded
    val parsed = markdownParser.parse(markdownEscaped)
    markdownRenderer.render(parsed)
  }
}
