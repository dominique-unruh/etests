package assessments

import assessments.DynexiteDefaults.{InputElementMethods, PageElementMethods, input}
import assessments.GradingContext.comments
import assessments.stack.StackMath
import org.scalatest.funsuite.AnyFunSuiteLike

class DynexiteDefaultsTest extends AnyFunSuiteLike {
  private def gc(x: String = null): GradingContext = {
    val map = Map.newBuilder[ElementName, String]
    if (x != null) map += ElementName("x") -> x
    GradingContext(map.result(), "123456", 10)
  }

  test("mathTry, valid parse") {
    given context: GradingContext = gc(x = "12 + 3")
    val x = input("15")
    val result = x.mathTry
    println(result)
    assert(result.toString == "plus(12, 3)")
    assert(comments.isEmpty)
  }

  test("mathTry, invalid parse") {
    given context: GradingContext = gc(x = "12 + ")
    val x = input("15")
    val result = x.mathTry
    println(result)
    assert(result == StackMath.noAnswer)
    assert(comments.length == 1)
    assert(comments.head.toPlaintext == "Could not parse x (error: Error parsing 12 + : '+' is an invalid final character in <span class=\"stacksyntaxexample\">12 +</span>), treating as no answer")
  }
}
