import assessments.{Exam, ExceptionContext}

import scala.util.Random

object ExamAccessDemo {
  def main(args: Array[String]): Unit = {
    given ExceptionContext = ExceptionContext.initialExceptionContext("Test program")

    // All exam IDs
    val examIds = Exam.exams.map(_.id)
    println(s"Exam IDs: ${examIds.mkString(", ")}")
    // Picking some exam ID
    val examId = examIds(Random.nextInt(examIds.length))
    println(s"Picked exam $examId")
    // Get the exam
    val exam = Exam.getExamById(examId)
    // Get all problem IDs in that exam
    val problemIds = exam.problems.map(_.id)
    println(s"Problem IDs: ${problemIds.mkString(", ")}")
    // Picking some problem ID
    val problemId = problemIds(Random.nextInt(problemIds.length))
    println(s"Picked problem $examId")
    val problem = exam.assessmentById(problemId)
    println(s"Retrieved: ${problem.name}")
    val (body, explanation, gradingRules, files) = problem.renderHtml
    println(s"Main HTML: ${body}")
    println(s"Files to be served alongside: ${files.keys.mkString(", ")}")
  }
}
