package externalsystems

import assessments.ExceptionContext
import exam.y2025.iqc1.{ApplyCNOTLike, Factor, Iqc1Exam}
import org.scalatest.funsuite.AnyFunSuiteLike
import utils.Utils

import scala.annotation.experimental
import scala.util.control.Breaks.{break, breakable}

class DynexiteTest extends AnyFunSuiteLike {
  Utils.loadSystemProperties()
  val exam: Iqc1Exam.type = Iqc1Exam

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
      val results = Dynexite.getDynexiteAnswers(Factor.assessment, Iqc1Exam, "***REMOVED***")
      print(results)
    }
  }
}
