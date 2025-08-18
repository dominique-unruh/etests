package assessments

import org.apache.commons.text.StringEscapeUtils
import utils.Tag.Tags

import scala.collection.SeqMap

class DummyAssessment(name: String) extends Assessment(
  name = name,
  questionTemplate = InterpolatedHtml(Html(s"DUMMY ASSESSMENT: ${StringEscapeUtils.escapeHtml4(name)}")),
  explanationTemplate = InterpolatedHtml.empty,
  gradingRulesTemplate = InterpolatedHtml.empty,
  pageElements = SeqMap.empty,
  reachablePoints = -1,
  tags = Tags()
)
