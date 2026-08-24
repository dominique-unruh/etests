package assessments

import assessments.GradingScale.{failGrade, passingGrades}
import assessments.InterpolatedMarkdown.md
import utils.Utils

import scala.util.boundary
import scala.util.boundary.break
import assessments.HtmlConvertible.extraConversions.points2html
import externalsystems.Spreadsheet
import cats.syntax.foldable.*

/** A mapping from the number of points reached in an exam to a final grade.
  *
  * Grades follow the German university scale where 1.0 is the best passing grade and
  * [[GradingScale.failGrade]] (5.0) means failed. */
trait GradingScale {
  /** Checks that this scale is well-formed for an exam worth `reachable` points, throwing an
    * [[AssertionError]] if not (e.g. points out of range or grades not strictly ordered).
    *
    * @param reachable the maximum number of points obtainable in the exam */
  def assertCorrect(reachable: Points): Unit
  /** Returns the final grade for a student who reached `points` points. */
  def grade(registrationNumber: String, points: Points): (Html, Double)
  /** An HTML table rendering of this scale, for display to students. */
  def html: Html
}

class BonusPointGrading(scale: GradingScale, passingThreshold: Points, reachableBonusPoints : Points,
                        bonusPointTable : => Spreadsheet) extends GradingScale {
  private lazy val bonusPointTableLazy = bonusPointTable
  private val bonusPointsIndex = Spreadsheet.Index("bonus points", "Matrikelnummer", (_, r) => Points(r("MANUALLY CORRECTED")))

  /** Checks that this scale is well-formed for an exam worth `reachable` points, throwing an
   * [[AssertionError]] if not (e.g. points out of range or grades not strictly ordered).
   *
   * @param reachable the maximum number of points obtainable in the exam */
  override def assertCorrect(reachable: Points): Unit = {
    assert(reachable == Points(100))
    scale.assertCorrect(reachable)
    assert(scale.grade("", passingThreshold)._2 == 4.0)
    assert(scale.grade("", passingThreshold - 0.000001)._2 == 5.0)
  }

  /** Returns the final grade for a student who reached `points` points. */
  override def grade(registrationNumber: String, points: Points): (Html, Double) = {
    val comments = Seq.newBuilder[InterpolatedMarkdown[Points]]
    val bonusPoints = bonusPointTable.lookup(bonusPointsIndex, registrationNumber)
    assert(bonusPoints <= reachableBonusPoints)
    val examPoints = points
    val examReachable = 100 : Points
    val adjustedPercentageForPassing = (examPoints/examReachable + bonusPoints/reachableBonusPoints * Points(6)/100) * 100
    val adjustedPercentageForGrade = (examPoints/examReachable + bonusPoints/reachableBonusPoints * Points(12)/100) * 100
    comments += md"* Bonus points: $bonusPoints"
    comments += md"* Bonus point percentage: ${bonusPoints/reachableBonusPoints * 100}%"
    comments += md"* Adjusted exam points with 6% bonus points (relevant for passing): ${adjustedPercentageForPassing}"
    comments += md"* Adjusted exam points with 12% bonus points (relevant for passing): ${adjustedPercentageForGrade}"
    val grade: Double = if (adjustedPercentageForGrade < passingThreshold) {
      comments += md"* Exam points for passing not above passing threshold ($passingThreshold), exam not passed"
      5.0
    } else {
      comments += md"* Enough points passing, grade computed based on the adjusted points for passing"
      val (_,grade) = scale.grade(registrationNumber, adjustedPercentageForGrade)
      grade
    }
    val comments2 = comments.result().intercalate(InterpolatedMarkdown.newline).toHtml.flatMapArgs(_.toHtml)
    (comments2, grade)
  }

  /** An HTML table rendering of this scale, for display to students. */
  override def html: Html = scale.html
}

/** A [[GradingScale]] defined by a list of point thresholds.
  *
  * Each entry `(points, grade)` means "at least `points` points earns `grade`". Entries must be
  * ordered by descending points (best grade first) with distinct grades. A student who does not
  * reach the lowest threshold gets [[GradingScale.failGrade]].
  *
  * @param grades the threshold/grade pairs, highest threshold first */
class SimpleGradingScale(grades: Seq[(Points, Double)]) extends GradingScale {
  def assertCorrect(reachable: Points): Unit = {
    assert(grades.nonEmpty)
    for ((points, grade) <- grades) {
      assert(passingGrades.contains(grade))
      assert(points >= 0)
      assert(points <= reachable)
    }
    assert(Utils.isIncreasing(grades.map(_._1))(using summon[Ordering[Points]].reverse))
    assert(Utils.isDistinct(grades.map(_._2)))
  }
  /** Returns a copy of this scale with an additional `(points, grade)` threshold appended. */
  def set(points: Points, grade: Double): SimpleGradingScale =
    SimpleGradingScale(grades appended (points -> grade))
  def grade(registrationNumber: String, points: Points): (Html, Double) = boundary {
    for ((needed, grade) <- grades)
      if (points >= needed)
        break((Html.empty, grade))
    (Html.empty, failGrade)
  }

  def html: Html = {
    val code = new StringBuilder()
    code ++= "<table class=\"grade-scale\">\n"
    code ++= "<tr><th colspan=\"2\">Grade scale</th></tr>\n"
    code ++= "<tr><th>Points</th><th>Grade</th></tr>\n"
    for ((points, grade) <- grades)
      code ++= s"<tr><td>$points</td><td>$grade</td></tr>\n"
    code ++= s"<tr><td>less</td><td>${failGrade}</td></tr>\n"
    code ++= "</table>\n"
    Html(code.result())
  }
}

object SimpleGradingScale {
  /** A scale with no thresholds; everything grades as [[GradingScale.failGrade]]. Use as a base for
    * building scales with [[SimpleGradingScale.set]]. */
  val empty = SimpleGradingScale(Seq.empty)
  /** The standard grading scale for an exam worth 100 points, with 50 points required to pass. */
  val defaultGradingScale100: GradingScale = empty
    .set(95, 1)
    .set(90, 1.3)
    .set(85, 1.7)
    .set(80, 2)
    .set(75, 2.3)
    .set(70, 2.7)
    .set(65, 3)
    .set(60, 3.3)
    .set(55, 3.7)
    .set(50, 4)
}

object GradingScale {
  /** All valid passing grades on the German scale, best (1.0) to worst passing (4.0). */
  val passingGrades: Seq[Double] = Seq(1, 1.3, 1.7, 2, 2.3, 2.7, 3, 3.3, 3.7, 4)
  /** The grade denoting a failed exam (5.0). */
  val failGrade: Double = 5
}