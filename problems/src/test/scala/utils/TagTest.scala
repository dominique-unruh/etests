package utils

import assessments.pageelements.InputElement
import org.scalatest.funsuite.AnyFunSuiteLike
import utils.Tag.Tags

class TagTest extends AnyFunSuiteLike {
  object tag1 extends Tag[Unit, Int](default = 10)
  object tag2 extends Tag[Unit, Int](default=11)

  test("lookup tag") {
    val tags = Tags[Unit](tag1 := 1, tag2 := 2)
    assert(tags(tag1) == 1)
    assert(tags(tag2) == 2)
  }
}
