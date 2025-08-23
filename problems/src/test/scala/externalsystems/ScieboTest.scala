package externalsystems

import org.scalatest.funsuite.AnyFunSuiteLike
import utils.Utils

import java.nio.file.Path

class ScieboTest extends AnyFunSuiteLike {
  test("Share") {
    // /apps/files/files
    val link = Sciebo.getPublicReadLink("tmp/sharetest")
    println(link)
//    println(link.getToken)
//    println(link.getUrl)
//    println(link.getId)
//    println(link.getShareTime)
  }
}
