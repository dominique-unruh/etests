package assessments

final class InterpolatedHtml[+T](val interpolatedString: InterpolatedString[T])
  extends InterpolatedText[T, Html, InterpolatedHtml] {

  override def mapArgs[U](f: T => U): InterpolatedHtml[U] =
    new InterpolatedHtml[U](interpolatedString.mapArgs(f))

  override def mapCompleteText(f: Html => Html): InterpolatedHtml[T] =
    new InterpolatedHtml[T](interpolatedString.mapCompleteText(str => f(Html(str)).html))

  override def checkCorrectness(): Unit =
    interpolatedString.checkCorrectness()

  override def isComplete: Boolean =
    interpolatedString.isComplete

  override def completeText: Html =
    Html(interpolatedString.completeText)

  override def flatMapArgs(f: T => Html): Html =
    Html(interpolatedString.flatMapArgs(t => f(t).html))

  override def args: Seq[T] =
    interpolatedString.args

  override def ++[U >: T](other: InterpolatedHtml[U]): InterpolatedHtml[U] =
    new InterpolatedHtml[U](interpolatedString ++ other.interpolatedString)
}

object InterpolatedHtml extends InterpolatedTextC[Html, InterpolatedHtml] {
//  extension (sc: StringContext) {
//    inline def md[T](args: T*): InterpolatedHtml[T] = new InterpolatedHtml(InterpolatedString[T](sc.parts, args))
//  }

//    extension (it: InterpolatedHtml[Html]) {
//      def mkText: Html = it.flatMapArgs(identity)
//    }

  override def apply[T](text: Html): InterpolatedHtml[T] =
    new InterpolatedHtml(InterpolatedString(text.html))

  val empty = new InterpolatedHtml(InterpolatedString.empty)
}

case class Html(html: String)

object Html {
  val empty: Html = Html("")
}