package assessments

import scala.annotation.targetName
import scala.collection.mutable

final class Commenter {
  private val builder = Seq.newBuilder[String]
  @targetName("addComment")
  def +=(comment: String): Unit = builder += comment
  def comments: Seq[String] = builder.result()
  def clear(): Unit = builder.clear()
}
