package assessments

import assessments.Comment.Kind.{debug, feedback}
import assessments.GradingContext.comments
import org.apache.commons.lang3.StringUtils
import org.scalatest.funsuite.AnyFunSuiteLike

import scala.collection.mutable

class CommentTest extends AnyFunSuiteLike {
  test("filter feedback") {
//    given GradingContext = GradingContext(Map(), "XXX", 0)
    val comments = Seq(Comment.feedback("feedback"), Comment.debug("debug"),
      NestedComment(Seq(Comment.feedback("feedback"), Comment.debug("debug")), kind=feedback),
      NestedComment(Seq(Comment.feedback("debug"), Comment.debug("debug")), kind=debug))
    println(comments.map(_.toPlaintext).mkString(";"))
    val filtered = Comment.filterFeedback(comments)
    val text = filtered.map(_.toPlaintext).mkString(";")
    println(text)
    assert(!text.contains("debug"))
    assert(StringUtils.countMatches(text, "feedback") == 2)
  }

}
