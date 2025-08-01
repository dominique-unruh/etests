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
    val xx = 123
    println(ts"hello\nthere$xx")
  }
}
