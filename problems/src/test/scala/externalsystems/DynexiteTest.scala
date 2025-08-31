package externalsystems

import assessments.{Exam, ExceptionContext}
import org.scalatest.funsuite.AnyFunSuiteLike
import utils.Utils

import scala.annotation.experimental
import scala.util.control.Breaks.{break, breakable}

class DynexiteTest extends AnyFunSuiteLike {
  Utils.loadSystemProperties()
  val exam: Exam = Utils.getSystemPropertyObject[Exam]("current.exam", "the current exam")

  private def resultsAvailable: Boolean = {
    try
      Dynexite.resultJsonPath(exam)
      true
    catch
      case _ =>
        println("No exam result file configured. Skipping test.")
        false
  }

  test("load results") {
    if (resultsAvailable) {
      Dynexite.resultsByLearner(exam)
  }}

  test("get answers") {
    given ExceptionContext = ExceptionContext.initialExceptionContext("Test case")
    if (resultsAvailable) {
      val regno = Dynexite.randomLearner(exam)
      val results = Dynexite.getDynexiteAnswers(exam.problems.head.assessment, exam, regno)
      print(results)
    }
  }
}
