package assessments

import assessments.pageelements.AnswerElement

import scala.collection.mutable

case class Answers(answers: Map[ElementName, String], description: String = "") {
  assert(description != null)
  override def toString: String = answers.map((k, v) => s"$k -> $v").mkString(", ")

  def update(changes: (AnswerElement, String)*)(using exceptionContext: ExceptionContext, name: ImplicitName[ElementName, ""]): Answers =
    update(changes, description = "solution " + name.name)

  def update(changes: Seq[(AnswerElement, String)], description: String = null)(using ExceptionContext): Answers = {
    val changedAnswers = mutable.Map(answers.toSeq *)
    for ((pageElement, value) <- changes)
      if (pageElement == null)
        throw ExceptionWithContext(s"Changed contain a null (the changed answer element)", value, changedAnswers)
      else
        changedAnswers(pageElement.name) = value

    val changedName =
      if (description == null) s"${this.description} with ${changes.map((k, v) => s"${k.name} -> $v").mkString(", ")}"
      else description
    Answers(changedAnswers.toMap, changedName)
  }
}