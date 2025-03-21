package assessments.stack

import assessments.stack.StackMath.{Operation, Ops}
import org.scalatest.funsuite.AnyFunSuiteLike

class StackParserTest extends AnyFunSuiteLike {
  test("parse: (m)/n") {
    val str = "(m)/n"
    val result = StackParser.parse(str)
  }
}
