package assessments.pageelements

import assessments.{Answers, Assessment, ElementName}
import utils.Tag
import utils.Tag.{Tagged, Tags}

final case class RenderContext(tags: Tags[RenderContext]) {
  def apply[Value](tag: Tag[RenderContext, Value]): Value = tags(tag)
  def get[Value](tag: Tag[RenderContext, Value]): Option[Value] = tags.get(tag)
  def getOrElse[Value](tag: Tag[RenderContext, Value], default: => Value): Value = 
    tags.get(tag).getOrElse(default)
  def +[Value](tag: Tagged[RenderContext, Value]) = RenderContext(tags + tag)
    
  def studentAnswer(name: ElementName): Option[String] =
    get(RenderContext.studentAnswers).flatMap(_.answers.get(name))
}

object RenderContext {
  def apply(tags: Tag.Tagged[? >: RenderContext, ?]*) = new RenderContext(Tags(tags*))
  
  /** Whether to render dynamic or static HTML */
  val dynamic: Tag[RenderContext, Boolean] = Tag(default = true)
  /** The answers the student gave */
  val studentAnswers: Tag[RenderContext, Answers] = Tag()
  /** The [[Assessment]] object we are rendering */
  val problem: Tag[RenderContext, Assessment] = Tag()
  /** Registration number of the student */
  val registrationNumber: Tag[RenderContext, String] = Tag()
  /** Whether to try and catch exceptions when rendering (and replace the failed elements by error messages),
   * or just throw the exceptions. */
  val catchExceptions: Tag[RenderContext, Boolean] = Tag(default = false)
  /** Whether solution-only content ([[assessments.pageelements.SolutionElement]]: explanations,
   * grading rules, graders) is included. Default `true`. Set `false` for a blank question sheet (e.g.
   * a student printout). Only affects **static** rendering (`dynamic := false`); in dynamic rendering
   * the `<etest-solution>` web component is always emitted and visibility is handled client-side. */
  val showSolutions: Tag[RenderContext, Boolean] = Tag(default = true)
}
