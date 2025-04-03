import assessments.ExceptionContext
import exam.PqcExam2

object ExamTestTest {
  def main(args: Array[String]): Unit = {
    given ExceptionContext = ExceptionContext.initialExceptionContext("Testing exam")
    val exam = PqcExam2
    for (question <- exam.questions) {
      given ExceptionContext = ExceptionContext.addToExceptionContext(s"Question ${question.name}", question)
      question.runTests()
    }
  }
}
