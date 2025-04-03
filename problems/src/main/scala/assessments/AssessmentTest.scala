package assessments

trait AssessmentTest {
  def runTest()(using exceptionContext: ExceptionContext): Unit
}
