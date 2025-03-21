package assessments

object Preamble {
  def multiplechoice(options:Map[String, String], correct:String, points:Points)(using context: Context): MultipleChoice =
    MultipleChoice(name=context.name, options=options, correct=correct, points=points)

  def textinput(correct: Seq[String], wrong: Seq[String], partiallyCorrect: Map[String,Points] = Map.empty, points: Points)(using context: Context): TextInput =
    TextInput(name=context.name, reference="unknown", correct = correct, wrong = wrong, points = points, partiallyCorrect = partiallyCorrect)

  def grader(python: String = null, points: Points, check: Map[ElementName, String] => Points|Boolean = null)(using context: Context): OldGrader =
    OldGrader(name=context.name, points=points, python=python, check=check)
}
