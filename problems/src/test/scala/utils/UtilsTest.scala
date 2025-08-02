package utils

import org.scalatest.funsuite.AnyFunSuiteLike

class UtilsTest extends AnyFunSuiteLike {

  test("stripLeadingEmptyLines") {
    val text = "\t\n  \t\n   test\nbla\n\n"
    val expected = "   test\nbla\n\n"
    assert(Utils.stripLeadingEmptyLines(text) == expected)
  }

}
