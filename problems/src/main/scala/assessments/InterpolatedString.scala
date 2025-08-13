package assessments;

import scala.StringContext;

case class InterpolatedString[+T] private (parts: Seq[String], args: Seq[T]) {
  def mapArgs[U](f: T => U) : InterpolatedString[U] =
    new InterpolatedString[U](parts, args.map(f))

  def mapCompleteString(f: String => String): InterpolatedString[T] = {
    val marker1 = '\uffef'
    val marker2 = '\ufeff'
    val numbers = args.zipWithIndex.map((_,index) => marker1.toString + marker2.toString * index)
    val stringWithNumbers = new InterpolatedString(parts, numbers).mkString
    val mappedStringWithNumbers = f(stringWithNumbers)
    val newParts = Seq.newBuilder[String]
    val newArgs = Seq.newBuilder[T]
    var index = 0
    val currentPart = new StringBuffer
    val len = mappedStringWithNumbers.length
    while (index < len) {
      val c = mappedStringWithNumbers(index)
      if (c == marker1) {
        index += 1
        var number = 0
        while (index < len && mappedStringWithNumbers(index) == marker2) {
          number += 1
          index += 1
        }
        newParts += currentPart.toString
        currentPart.setLength(0)
        assert(number < args.length)
        newArgs += args(number)
      } else {
        currentPart.append(c)
        index += 1
      }
    }
    newParts += currentPart.toString
    new InterpolatedString[T](newParts.result(), newArgs.result())
  }

  def checkCorrectness(): Unit =
    assert (parts.length == args.length + 1)
}

object InterpolatedString {
  def apply[T](parts: Seq[String], args: Seq[T]): InterpolatedString[T] = {
    val is = new InterpolatedString[T](parts, args)
    is.checkCorrectness()
    is
  }

  def apply[T](string: String) = new InterpolatedString[T](Seq(string), Seq.empty)
  
  val empty = InterpolatedString("")
  
  extension (is: InterpolatedString[String]) {
    def mkString: String = is.parts.zipAll(is.args, "", "").map((p,a) => p+a).mkString
  }
}