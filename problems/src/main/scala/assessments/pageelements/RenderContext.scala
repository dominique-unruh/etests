package assessments.pageelements

import assessments.ElementName
import utils.Tag
import utils.Tag.Tags

final case class RenderContext(tags: Tags[RenderContext]) {
  def apply[Value](tag: Tag[RenderContext, Value]): Value = tags(tag)
  def get[Value](tag: Tag[RenderContext, Value]): Option[Value] = tags.get(tag)
  def getOrElse[Value](tag: Tag[RenderContext, Value], default: => Value): Value = 
    tags.get(tag).getOrElse(default)
    
  def studentAnswer(name: ElementName): Option[String] =
    get(RenderContext.studentAnswers).flatMap(_.get(name))
}

object RenderContext {
  def apply(tags: Tag.Tagged[? >: RenderContext, ?]*) = new RenderContext(Tags(tags*))
  
  /** Whether to render dynamic or static HTML */
  val dynamic: Tag[RenderContext, Boolean] = Tag(default = true)
  /** The answers the student gave */
  val studentAnswers: Tag[RenderContext, Map[ElementName, String]] = Tag()
}