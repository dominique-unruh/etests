package assessments

case class Exam(name: String, problems: Assessment*) {
  assert(problems.map(_.name).distinct.length == problems.map(_.name).length)
  
  def assessmentIndex(assessment: Assessment)(implicit exceptionContext: ExceptionContext): Int = {
    given ExceptionContext = ExceptionContext.addToExceptionContext(s"Looking for assessment $assessment in exam", assessment, this)
    val index = problems.indexWhere(_ eq assessment)
    if (index == -1)
      throw ExceptionWithContext(s"Assessment $assessment not found in exam ${this.name}")
    index
  }
}
