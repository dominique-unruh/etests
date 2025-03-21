package assessments.stack

import assessments.stack.StackParser.parse
import assessments.stack.StackUtils.{checkEquality, sympy}
import org.scalatest.funsuite.AnyFunSuiteLike

class StackUtilsTest extends AnyFunSuiteLike {

  test("checkEquality with sqrt") {
    val t1 = parse("2^((1-c/2)*n)").toSympy
    val t2 = parse("sqrt(2^(2*n-n*c))").toSympy
    assert(checkEquality(t1,t2))
  }
}
