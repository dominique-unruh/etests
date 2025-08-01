import org.commonmark.node.Node
import org.commonmark.parser.{InlineParser, InlineParserContext, InlineParserFactory, Parser, SourceLines}
import org.commonmark.renderer.html.HtmlRenderer

object Tmp {
  extension (sc: StringContext) {
    def ts(args: Any*): String = {
      for (p <- sc.parts) p match
        case p: String => println("Str: "+StringContext.processEscapes(p))
        case p => println(p.getClass)
      "nothing"
    }
  }

  def main(args: Array[String]): Unit = {

  }
}
