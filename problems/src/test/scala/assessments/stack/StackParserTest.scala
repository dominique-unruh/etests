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

  test("don't simplify") {
    val result = StackParser.parse("2+3")
    assert(result == Operation(Ops.plus, 2, 3))
  }

  test("a+b+c+d") {
    val result = StackParser.parse("a+b+c+d")
    assert(result.toString == "plus(plus(plus(a, b), c), d)")
  }

  test("a+(b+c)+d") {
    val result = StackParser.parse("a+(b+c)+d")
    println(result.toString)
    assert(result.toString == "plus(plus(a, plus(b, c)), d)")
  }

}
