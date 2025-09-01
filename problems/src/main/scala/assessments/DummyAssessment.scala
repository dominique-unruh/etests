package assessments

import scala.language.implicitConversions

import assessments.InterpolatedMarkdown.md
import assessments.pageelements.Element
import org.apache.commons.text.StringEscapeUtils
import utils.Tag.Tags

import scala.collection.SeqMap

class DummyAssessment(override val name: String) extends MarkdownAssessment {
  assert(name != null && name != "")
  override lazy val question: InterpolatedMarkdown[Element] = Markdown("Dummy question $name")

  override def grade()(using context: GradingContext, exceptionContext: ExceptionContext): Unit =
    throw NoGraderYetException

  override val reachablePoints: Points = 999
}
