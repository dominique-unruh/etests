package assessments

import org.scalatest.funsuite.AnyFunSuiteLike

class InterpolatedStringTest extends AnyFunSuiteLike {
  test("mapCompleteString") {
    println("START")
    val is = InterpolatedString(Seq("this", "is test", "end"), Seq(1,2))
    println(is)
    def f(string: String) = {
      string.split(" ").reverse.mkString(" ")
    }
    val is2 = is.mapCompleteText(f)
    println(is2.parts)
    println(is2.args)
    is2.checkCorrectness()
    val expected = InterpolatedString(Seq("test","end this","is"), Seq(2,1))
    assert(is2 == expected)
  }
}
