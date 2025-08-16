package assessments

final class InterpolatedMarkdown[+T](val interpolatedString: InterpolatedString[T])
  extends InterpolatedText[T, Markdown, InterpolatedMarkdown] {

  override def mapArgs[U](f: T => U): InterpolatedMarkdown[U] =
    new InterpolatedMarkdown[U](interpolatedString.mapArgs(f))

  override def mapCompleteText(f: Markdown => Markdown): InterpolatedMarkdown[T] =
    new InterpolatedMarkdown[T](interpolatedString.mapCompleteText(str => f(Markdown(str)).markdown))

  override def checkCorrectness(): Unit =
    interpolatedString.checkCorrectness()

  override def isComplete: Boolean =
    interpolatedString.isComplete

  override def completeText: Markdown =
    Markdown(interpolatedString.completeText)

  override def flatMapArgs(f: T => Markdown): Markdown =
    Markdown(interpolatedString.flatMapArgs(t => f(t).markdown))

  override def args: Seq[T] =
    interpolatedString.args
}

object InterpolatedMarkdown extends InterpolatedTextC[Markdown, InterpolatedMarkdown] {
  extension (sc: StringContext) {
    inline def md[T](args: T*): InterpolatedMarkdown[T] = new InterpolatedMarkdown(InterpolatedString[T](sc.parts, args))
  }

  override def apply[T](text: Markdown): InterpolatedMarkdown[T] =
    new InterpolatedMarkdown(InterpolatedString(text.markdown))
}

case class Markdown(markdown: String)
