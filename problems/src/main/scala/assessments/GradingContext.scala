package assessments

import scala.language.implicitConversions
import assessments.pageelements.AnswerElement

import scala.collection.mutable

/** Contains all information (including mutable one) that is relevant when grading a single grading
 * rule (except for the problem itself).
 *
 * A grader reads the student's [[answers]] (and may rewrite them for cleanup), may add [[comments]],
 * and refers to the reachable points of its rule as [[GradingContext.max]]. It returns its verdict as
 * a [[GradingContext.GraderOutcome]] (it does not award points directly — the points follow from the
 * outcome and the rule's configuration; see [[pageelements.GradingElement]]).
 *
 * Not thread safe!
 * */
case class GradingContext private (private val answers: mutable.Map[ElementName, String], val registrationNumber: String,
                                   private val reachable: Points,
                                   val assessment: MarkdownAssessment) {

  /** Reachable points for the current grading rule. */
  def reachablePoints: Points = reachable

  private val comments: mutable.IndexedBuffer[Comment] = mutable.ArrayDeque[Comment]()
}

object GradingContext {
  /** A grader's verdict about a student's answer for a single grading rule. The awarded points follow
   * from this outcome together with the rule's configuration (`reachablePoints`, `negative`); a grader
   * never manipulates points directly.
   *
   *  - [[fires]]: the rule applies fully — the rule's full points are added (positive rule) or
   *    subtracted (negative rule).
   *  - [[firesPartially]]: the rule applies partially — `points` are added/subtracted; must satisfy
   *    `0 < points < reachablePoints` (checked when the outcome is used, else an exception is thrown).
   *  - [[doesntFire]]: the rule does not apply — no points. */
  enum GraderOutcome {
    case fires
    case firesPartially(points: Points)
    case doesntFire

    /** Whether this outcome counts as the rule having "fired" (fully or partially). Used to decide
     * `unless` suppression and mutual-exclusion tests. */
    def fired: Boolean = this match {
      case doesntFire => false
      case _ => true
    }
  }

  /** The visual badge shown for a grading rule in the solution/grading view. Decoupled from
   * [[GraderOutcome]]: the same outcome maps to different badges depending on whether the rule is
   * positive/negative and full/partial. See [[pageelements.GradingElement.displayFor]].
   *
   *  - [[correct]]: green check (positive full rule fired).
   *  - [[partial]]: yellow circle (partial rule fired, or a rule fired only partially).
   *  - [[incorrect]]: red cross (positive rule did not fire).
   *  - [[penalized]]: red check (negative full rule fired — a penalty was applied).
   *  - [[noPenalty]]: green cross (negative rule did not fire — no penalty).
   *  - [[notApplicable]]: gray n/a (rule not evaluated because an `unless` rule fired).
   *  - [[error]]: the grader threw. */
  enum DisplayOutcome {
    case correct, partial, incorrect, penalized, noPenalty, notApplicable, error

    /** Short badge glyph (the color conveys good/bad; see `_solution.scss` / `grading.ts`). */
    def glyph: String = this match {
      case DisplayOutcome.correct | DisplayOutcome.penalized => "✓"  // check (green=correct, red=penalty applied)
      case DisplayOutcome.partial => "◐"                             // half circle
      case DisplayOutcome.incorrect | DisplayOutcome.noPenalty => "✗" // ballot X (red=wrong, green=no penalty)
      case DisplayOutcome.notApplicable => "n/a"
      case DisplayOutcome.error => "⚠"                               // warning sign
    }
  }

  def comments(using context: GradingContext): mutable.IndexedBuffer[Comment] = context.comments

  def answers(using context: GradingContext): mutable.Map[ElementName | AnswerElement, String] = new mutable.Map {
    private val answers = context.answers
    private def toName(name: ElementName | AnswerElement): ElementName = name match
      case name: ElementName => name
      case element: AnswerElement => element.name
    override def addOne(elem: (ElementName | AnswerElement, String)): this.type = {
      answers.addOne((toName(elem._1), elem._2)); this
    }
    override def get(key: ElementName | AnswerElement): Option[String] = answers.get(toName(key))
    override def iterator: Iterator[(ElementName, String)] = answers.iterator
    override def subtractOne(elem: ElementName | AnswerElement): this.type = {
      answers.subtractOne(toName(elem)); this
    }
  }
  def answersImmutable(using context: GradingContext): Answers =
    Answers(answers.toMap.map { (k,v) => (k match { case k:ElementName => k; case k:AnswerElement => k.name }, v) })

  def apply(answers: Map[ElementName, String], registrationNumber: String, reachable: Points,
            assessment: MarkdownAssessment): GradingContext =
    new GradingContext(answers.to(mutable.Map), registrationNumber, reachable, assessment = assessment)

  /** Reachable points for the current grading rule (to be used inside a grader). */
  def max(using context: GradingContext): Points = context.reachable

  @deprecated("Just use `answers(name) = ...` or `answers.updateWith(...)`.")
  def fixAnswer(name: ElementName)(f: PartialFunction[String, String])(using gradingContext: GradingContext): Unit =
    f.lift(answers(name)) match
      case Some(value) => answers(name) = value
      case None =>

  @deprecated("Just use `answers(name) = ...` or `answers.updateWith(...)`.")
  def fixAnswer(element: AnswerElement)(f: PartialFunction[String, String])(using gradingContext: GradingContext): Unit =
    fixAnswer(element.name)(f)
}