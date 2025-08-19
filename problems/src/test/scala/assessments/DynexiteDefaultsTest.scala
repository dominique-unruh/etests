package assessments

import assessments.DynexiteDefaults.{PageElementMethods, input}
import assessments.stack.StackMath
import org.scalatest.funsuite.AnyFunSuiteLike

class DynexiteDefaultsTest extends AnyFunSuiteLike {
  private def gc(x: String = null): GradingContext = {
    val map = Map.newBuilder[ElementName, String]
    if (x != null) map += ElementName("x") -> x
    GradingContext(map.result(), "123456")
  }

  test("mathTry, valid parse") {
    given GradingContext = gc(x = "12 + 3")
    given commenter: Commenter = Commenter()
    val x = input("15")
    val result = x.mathTry("x")
    println(result)
    assert(result.toString == "plus(12, 3)")
    assert(commenter.comments.isEmpty)
  }

  test("mathTry, invalid parse") {
    given GradingContext = gc(x = "12 + ")
    given commenter: Commenter = Commenter()
    val x = input("15")
    val result = x.mathTry("x")
    println(result)
    assert(result == StackMath.noAnswer)
    assert(commenter.comments.length == 1)
    assert(commenter.comments.head.toPlaintext == "Could not parse x, treating as no answer")
  }
}
