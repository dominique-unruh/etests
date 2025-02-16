package assessments

object Preamble {
  def multiplechoice(options:Map[String, String], correct:String, points:Points)(using context: Context): MultipleChoice =
    MultipleChoice(name=context.name, options=options, correct=correct, points=points)
    
  def mathinput(correct: String)(using context: Context): MathInput =
    MathInput
}
