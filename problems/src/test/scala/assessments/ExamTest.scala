package assessments

import exam.y2025.iqc1.Iqc1Exam
import org.scalatest.funsuite.AnyFunSuiteLike

class ExamTest extends AnyFunSuiteLike {
  test("Exam.exams") {
    val exams = Exam.exams
    println(exams.map(_.getClass.getSimpleName))
    assert(Exam.exams.contains(Iqc1Exam))
  }
}
