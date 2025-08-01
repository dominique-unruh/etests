package utils

implicit class IndentedInterpolator(val sc: StringContext) extends AnyVal {
  private inline def stripPipe(s: String): String = {
    val pos = s.indexOf('|')
    if (pos > 0) s.substring(pos + 1)
    else s
  }

  private inline def indentArg(arg: Any, indent: Int, sb: StringBuilder): Int = {
//    println(arg)
    val argString = arg.toString
    if (argString.isEmpty) indent
    else {
      var first = true
      var lastLine: String = null
      for (line <- argString.linesWithSeparators) {
        if (!first) {
          //          println(s"indent $indent")
          for (_ <- 0 until indent) {
            sb += ' '
          }
        }
        first = false
        sb ++= line
        lastLine = line
      }
      if (lastLine.nonEmpty && lastLine.last == '\n')
        indent
      else
        indent + lastLine.length
    }
  }

  def ind(args: Any*): String = {
    StringContext.checkLengths(args, sc.parts)
    val result = StringBuilder()
    var indent: Int = 0
    for ((part, arg) <- sc.parts.zipAll(args, "", "")) {
      var first = true
      for (line <- StringContext.processEscapes(part).linesWithSeparators) {
        val stripped = stripPipe(line)
        result ++= stripped
        if (first)
          indent += stripped.length
          first = false
        else
          indent = stripped.length
      }
      indent += indentArg(arg, indent, result)
    }
    result.toString()
  }
}
