package tmp

import org.commonmark.node.Node
import org.commonmark.parser.*
import org.commonmark.renderer.html.HtmlRenderer
import tmp.Tmp.getClass

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
    println(getClass.getName)
    println(getClass.getSimpleName)
    println(getClass.getTypeName)
    println(getClass.getCanonicalName)
    println(getClass.getPackageName)
  }
}
