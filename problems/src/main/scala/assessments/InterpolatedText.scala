package assessments

trait InterpolatedText[+T, Text, Self[+U] <: InterpolatedText[U, Text, Self]] {
  def mapArgs[U](f: T => U): Self[U]
  def mapCompleteText(f: Text => Text): Self[T]
  def checkCorrectness(): Unit
  def isComplete: Boolean
  def completeText: Text
  def flatMapArgs(f: T => Text): Text
  def args: Seq[T]
}

trait InterpolatedTextC[Text, Self[+U] <: InterpolatedText[U, Text, Self]] {
  def apply[T](text: Text): Self[T]

//  implicit class InterpolatedTextCMethods(self: Self[Text]) {
//    def mkText: Text = self.flatMapArgs(identity)
//  }

  extension (it: Self[Text]) {
    def mkText: Text = it.flatMapArgs(identity)
  }
}

