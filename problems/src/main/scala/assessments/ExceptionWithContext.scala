package assessments

class ExceptionWithContext(message: String, extraData: Any*)(implicit context: ExceptionContext) extends Exception {
  override def getMessage: String = {
    val longMessage = StringBuilder(message)
    for ((message,_) <- context.messages)
      longMessage.append("\n  In: ").append(message)
    longMessage.result()
  }
}

// TODO: Look whether org.apache.commons.lang3.exception.ExceptionContext is useful instead
class ExceptionContext private (val messages: List[(String, Seq[Any])]) {
  def add(message: String, extraData: Any*) =
    ExceptionContext((message, extraData) :: messages)
}

object ExceptionContext {
  def initialExceptionContext(message: String, extraData: Any*) = ExceptionContext(List((message, extraData)))
  def addToExceptionContext(message: String, extraData: Any*)(using exceptionContext: ExceptionContext): ExceptionContext = exceptionContext.add(message, extraData)
}
