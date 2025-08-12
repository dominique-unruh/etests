package assessments.stack

import assessments.DynexiteDefaults.sympy
import assessments.stack.SympyExpr.{integer, symbol}
import org.scalatest.funsuite.AnyFunSuiteLike

class SympyExprTest extends AnyFunSuiteLike {
  test("replaceFunction 1 arg") {
    val f = "f(f(x))".sympy
    val result = f.replaceFunction("f", x => SympyExpr.function("g")(x))
    val expected = "g(g(x))".sympy
    println(result)
    assert(result == expected)
  }

  test("replaceFunction 2 args") {
    val f = "f(f(x,y),y)".sympy
    val result = f.replaceFunction("f", (x,y) => SympyExpr.function("g")(x,y))
    val expected = "g(g(x,y),y)".sympy
    println(result)
    assert(result == expected)
  }

  test("gcd, explicit") {
    val x = integer(6).gcd(integer(15))
    println(x)
    assert(x.toString == "3")
  }

  test("gcd, with variable") {
    val x = symbol("x").gcd(integer(15))
    println(x)
    assert(x.toString == "gcd(x, 15)")
  }

  test("gcd, with variable, substitute") {
    val x = symbol("x").gcd(integer(15))
    println(x)
    val y = x.substitute("x" -> integer(6))
    println(y)
    assert(y.toString == "3")
  }
}
