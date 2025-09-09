package utils

import assessments.ExceptionContext
import org.scalatest.funsuite.AnyFunSuiteLike

import scala.concurrent.duration.Duration

class UtilsTest extends AnyFunSuiteLike {

  test("stripLeadingEmptyLines") {
    val text = "\t\n  \t\n   test\nbla\n\n"
    val expected = "   test\nbla\n\n"
    assert(Utils.stripLeadingEmptyLines(text) == expected)
  }

  test("stripTrailingEmptyLines") {
    val text = "\t\n  \t\n   test\nbla \n\t \n  "
    val expected = "\t\n  \t\n   test\nbla "
    assert(Utils.stripTrailingEmptyLines(text) == expected)
  }

  test("timeout, fast") {
    given ExceptionContext = ExceptionContext.initialExceptionContext("test case")
    val result = Utils.runWithTimeout(Duration("10s"), 5)
    assert(result == 5)
  }

  test("timeout, slow") {
    given ExceptionContext = ExceptionContext.initialExceptionContext("test case")
    @volatile var tralala = false
    assertThrows[Utils.Timeout] {
      Utils.runWithTimeout(Duration("1s"), { Thread.sleep(2000); println("tralala"); tralala = true; 5 })
    }
    Thread.sleep(3000)
    assert(!tralala)
  }
}
