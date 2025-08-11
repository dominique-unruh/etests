package assessments

import assessments.pageelements.{ElementAction, PageElement}
import play.api.libs.json.JsValue

abstract class Grader(val name: ElementName) extends PageElement {
  override def renderHtml: String = ""
  def grade(gradingContext: GradingContext): (Points, Seq[String])
  lazy val points: Points

  override def updateAction(assessment: Assessment, state: Map[ElementName, JsValue]): IterableOnce[ElementAction] =
    Seq.empty
}
