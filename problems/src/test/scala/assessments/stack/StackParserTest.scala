package assessments.stack

import assessments.stack.StackMath.{Operation, Ops}
import org.scalatest.funsuite.AnyFunSuiteLike

class StackParserTest extends AnyFunSuiteLike {
  test("parse: (m)/n") {
    val str = "(m)/n"
    val result = StackParser.parse(str)
  }

  /*test("tmp") {
    import StackParser.*
    import fastparse.*
    import MultiLineWhitespace.*
    val str = "(m)/n"

    def ba[$:P]: P[StackMath] = ("(" ~ variable ~ ")")
    def atom[$: P]: P[StackMath] = ("(" ~ variable ~ ")")
    def mult_like[$: P]: P[StackMath] =
      (atom ~ LiteralStr("/") ~ variable) map { (a,b) => Operation(Ops.divide, a, b) }
    def tmp[$: P]: P[StackMath] =
      ba |
        mult_like


    val result = fastparse.parse(str, { (p: P[?]) => given P[?] = p; tmp ~ End }, verboseFailures = true)
      .get.value

//    val result = StackParser.parseWith(str, tmp)
    println(result)
  }*/

}
