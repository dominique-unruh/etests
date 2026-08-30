package assessments.pageelements

/** Marker for page elements that are shown **only** in the exam solution (explanations, grading
 * rules, graders). It carries no implementation and does not extend [[Element]]: the two concrete
 * kinds do not share code and pick their own base.
 *  - [[ExplanationElement]] is a [[StaticElement]] that renders fixed explanation HTML.
 *  - [[GradingElement]] is a [[DynamicElement]] that grades and renders live feedback. */
trait SolutionElement
