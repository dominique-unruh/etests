package assessments

import assessments.Exam.logger
import assessments.ExceptionContext.{addToExceptionContext, initialExceptionContext}
import com.typesafe.scalalogging.Logger

case class Exam(name: String, problems: MarkdownAssessment*) {
  assert(problems.map(_.name).distinct.length == problems.map(_.name).length)
  
  def assessmentIndex(assessment: Assessment)(implicit exceptionContext: ExceptionContext): Int = {
    given ExceptionContext = ExceptionContext.addToExceptionContext(s"Looking for assessment $assessment in exam", assessment, this)
    val index = problems.indexWhere(_.assessment eq assessment)
    if (index == -1)
      throw ExceptionWithContext(s"Assessment ${assessment.name} not found in exam ${this.name} (did you include the question in the exam object?)")
    index
  }

  def assessmentByName(name: String)(implicit exceptionContext: ExceptionContext): MarkdownAssessment = {
    given ExceptionContext = ExceptionContext.addToExceptionContext(s"Looking for assessment $name in exam ${this.name}", name, this)
    val assessment = problems.find(_.name == name)
    assessment.getOrElse {
      throw ExceptionWithContext(s"Assessment ${name} not found in exam ${this.name} (did you include the question in the exam object?)")
    }
  }

  def runTests(): Unit = {
    given ExceptionContext = initialExceptionContext(s"Running tests for exam $name")
    for (assessment <- problems)
      assessment.runTests()
  }
  
  def main(args: Array[String]): Unit = {
    runTests()
  } 
}

object Exam {
  private val logger = Logger[Exam]
}