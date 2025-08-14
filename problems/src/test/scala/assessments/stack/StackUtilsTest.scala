package assessments.stack

import assessments.MathContext
import assessments.MathContext.mathContext
import assessments.stack.StackParser.parse
import assessments.stack.StackUtils.{checkEquality, checkEqualityNew}
import org.scalatest.funsuite.AnyFunSuiteLike
import utils.Python

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

  test("checkEqualityNew with test values, eq") {
    given MathContext = mathContext
      .testValues("x", 1, 2, 3, -12)
      .testValues("y", 1, 2, 3, -12)
    val t1 = parse("x+y")
    val t2 = parse("y+x")
    assert(checkEqualityNew(t1, t2))
  }

  test("checkEqualityNew with test values, neq") {
    given MathContext = mathContext
      .testValues("x", 1, 2, 3, -12)
      .testValues("y", 1, 2, 3, -12)
    val t1 = parse("x+y")
    val t2 = parse("y+y")
    assert(!checkEqualityNew(t1, t2))
  }


  test("checkEqualityNew with custom function") {
    val python = Python.defineFunction("f", "def f(x,y): return x+y")
    given MathContext = mathContext
      .sympyFunction("f", python, 2)
    val t1 = parse("f(2,3)")
    assert(checkEqualityNew(t1, 5))
  }

}
