package externalsystems

import externalsystems.MoodleStack.defaultForbiddenWords
import org.scalatest.funsuite.AnyFunSuiteLike

class MoodleStackTest extends AnyFunSuiteLike {
  test("defaultForbiddenWords") {
    println(defaultForbiddenWords.toSeq.sorted)
    assert(defaultForbiddenWords.contains("Y"))
    assert(defaultForbiddenWords.contains("xP"))
  }
}
