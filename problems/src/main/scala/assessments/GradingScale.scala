package assessments

import assessments.GradingScale.{failGrade, passingGrades}
import utils.Utils

import scala.util.boundary
import scala.util.boundary.break

class GradingScale(grades: Seq[(Points, Double)]) {
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
  def set(points: Points, grade: Double): GradingScale =
    GradingScale(grades appended (points -> grade))
  def grade(points: Points): Double = boundary {
    for ((needed, grade) <- grades)
      if (points >= needed)
        break(grade)
    failGrade
  }

  def html: Html = {
    val code = new StringBuilder()
    code ++= "<table class=\"grade-scale\">\n"
    code ++= "<tr><th colspan=\"2\">Grade scale</th></tr>\n"
    code ++= "<tr><th>Points</th><th>Grade</th></tr>\n"
    for ((points, grade) <- grades)
      code ++= s"<tr><td>${points}</td><td>${grade}</td></tr>\n"
    code ++= s"<tr><td>less</td><td>${failGrade}</td></tr>\n"
    code ++= "</table>\n"
    Html(code.result())
  }
}

object GradingScale {
  val passingGrades: Seq[Double] = Seq(1, 1.3, 1.7, 2, 2.3, 2.7, 3, 3.3, 3.7, 4)
  val failGrade: Double = 5
  val empty = GradingScale(Seq.empty)
  val defaultGradingScale100: GradingScale = empty
    .set(95, 1)
    .set(90, 1.3)
    .set(85, 1.7)
    .set(80, 2)
    .set(75, 2.3)
    .set(65, 3)
    .set(60, 3.3)
    .set(55, 3.7)
    .set(50, 4)
}