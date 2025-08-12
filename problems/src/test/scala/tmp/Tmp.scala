package tmp

import org.commonmark.node.Node
import org.commonmark.parser.*
import org.commonmark.renderer.html.HtmlRenderer
import tmp.Tmp.getClass
import utils.Utils

object Tmp {
  def main(args: Array[String]): Unit = {
    Utils.loadSystemProperties()
    println(System.getProperties)
  }
}
