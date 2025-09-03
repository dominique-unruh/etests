package assessments

import assessments.DynexiteDefaults.{InputElementMethods, PageElementMethods, input}
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
    val result = x.mathTry("x")
    println(result)
    assert(result.toString == "plus(12, 3)")
    assert(context.comments.isEmpty)
  }

  test("mathTry, invalid parse") {
    given context: GradingContext = gc(x = "12 + ")
    val x = input("15")
    val result = x.mathTry("x")
    println(result)
    assert(result == StackMath.noAnswer)
    assert(context.comments.length == 1)
    assert(context.comments.head.toPlaintext == "Could not parse x, treating as no answer")
  }
}
