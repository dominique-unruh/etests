package assessments

import cats.Monoid

import scala.collection.mutable

/** Common interface for different variants of interpolated text.
 * See [[InterpolatedString]] for an explanation.
 * */
trait InterpolatedText[+T, Text, Self[+U] <: InterpolatedText[U, Text, Self]] {
  def mapArgs[U](f: T => U): Self[U]
  def mapCompleteText(f: Text => Text): Self[T]
  def checkCorrectness(): Unit
  def isComplete: Boolean
  def completeText: Text
  /** Collapses this into a single plain [[Text]]: each interpolated arg is turned into `Text` by `f`
   * and spliced between the literal parts, then everything is concatenated. Use this to fully render
   * an interpolated text once every arg can be reduced to raw text. */
  def flatMapArgs(f: T => Text): Text
  /** Monadic flatMap over the args: each arg is replaced by another interpolated text (`f(arg)`),
   * whose parts and args are spliced in place, flattening the result into one interpolated text of
   * the same kind with new arg type `U`. Use this to substitute args while staying interpolated
   * (as opposed to the `T => Text` overload, which reduces to plain text). */
  def flatMapArgs[U](f: T => Self[U]): Self[U]
  def args: Seq[T]
  def ++[U >: T](other: Self[U]): Self[U]
}

trait InterpolatedTextC[Text, Self[+U] <: InterpolatedText[U, Text, Self]] {
  def apply[T](text: Text): Self[T]
  
  def fromArg[T](arg: T): Self[T]

  def empty: Self[Nothing]
  
  extension (it: Self[Text]) {
    def mkText: Text = it.flatMapArgs(identity)
  }

  given monoid[T]: Monoid[Self[T]] = new Monoid[Self[T]] {
    def empty: Self[T] = InterpolatedTextC.this.empty
    def combine(x: Self[T], y: Self[T]): Self[T] = x ++ y
  }
}

