package assessments.stack

import assessments.stack.StackParser.parse
import assessments.stack.StackUtils.{checkEquality}
import org.scalatest.funsuite.AnyFunSuiteLike

class StackUtilsTest extends AnyFunSuiteLike {
  test("checkEquality with sqrt") {
    val t1 = parse("2^((1-c/2)*n)").toSympy
    val t2 = parse("sqrt(2^(2*n-n*c))").toSympy
    assert(checkEquality(t1,t2))
  }

  test("checkEquality with mod 2") {
    val assumption = SympyAssumption.positive & SympyAssumption.integer
    val t1 = parse("f(x)+x+f(x)+x").toSympy % 2
    val t2 = 0 : SympyExpr
    assert(checkEquality(t1, 0, assumption))
  }
}
