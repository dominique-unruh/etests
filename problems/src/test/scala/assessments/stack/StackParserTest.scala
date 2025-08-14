package assessments.stack

import assessments.stack.StackMath.{Operation, Ops}
import org.scalatest.funsuite.AnyFunSuiteLike

class StackParserTest extends AnyFunSuiteLike {
  test("parse: (m)/n") {
    val str = "(m)/n"
    val result = StackParser.parse(str)
    println(result)
    assert(result.toString == "divide(m, n)")
  }

  test("parse: (-1)^n") {
    val str = "(-1)^n"
    val result = StackParser.parse(str)
    println(result)
    assert(result.toString == "power(unaryMinus(1), n)")
  }
}
