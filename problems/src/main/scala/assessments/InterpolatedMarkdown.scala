package assessments

import utils.Markdown.markdownToHtml

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

  override def ++[U >: T](other: InterpolatedMarkdown[U]): InterpolatedMarkdown[U] =
    new InterpolatedMarkdown[U](interpolatedString ++ other.interpolatedString)

  def toHtml: InterpolatedHtml[T] =
    new InterpolatedHtml(interpolatedString.mapCompleteText(markdownToHtml))
}

object InterpolatedMarkdown extends InterpolatedTextC[Markdown, InterpolatedMarkdown] {
  extension (sc: StringContext) {
    // Escape sequences show up as errors in Intelli/J IDEA. Bug report files: https://youtrack.jetbrains.com/issue/SCL-25082/Scala-Editor-shows-errors-in-correct-custom-interpolated-strings
    inline def md[T](args: T*): InterpolatedMarkdown[T] = new InterpolatedMarkdown(InterpolatedString[T](sc.parts, args))
  }

  override def apply[T](text: Markdown): InterpolatedMarkdown[T] =
    new InterpolatedMarkdown(InterpolatedString(text.markdown))

  given [T]: Conversion[Markdown, InterpolatedMarkdown[T]] = 
    md => InterpolatedMarkdown.apply(md)
}

/** Encapsulates Markdown code.
 * No well-formedness etc. is guaranteed.
 * (E.g., code could be something invalid like `ab *cd`.)
 * It is merely a thin wrapper to make it easier to make sure one does not mix up what
 * functions take HTML and what functions plaintext and what functions Markdown.
 *
 * @see [[Html]], [[Plaintext]]
 **/
final case class Markdown(markdown: String) extends AnyVal {
  def toHtml: Html = Html(markdownToHtml(markdown))
}