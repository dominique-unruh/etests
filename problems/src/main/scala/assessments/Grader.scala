package assessments

import me.shadaj.scalapy.py

class Grader(val name: ElementName, val points: Points,
             python: String = null,
             check: Map[ElementName, String] => Points|Boolean = null)
  extends PageElement {
  assert(List(python, check).count(_ != null) == 1)
  
  def grade(answers: Map[ElementName, String]): Points = {
    if (python!=null)
      gradePython(answers)
    else if (check!=null)
      gradeCheck(answers)
    else
      throw AssertionError("No checker provided")
  }
  
  def gradeCheck(answers: Map[ElementName, String]): Points = {
    check(answers) match {
      case true => points
      case false => 0
      case p:Points => p
    }
  }
  
  def gradePython(answers: Map[ElementName, String]): Points = {
    val result = Python.execLocally(python, Map("answers" -> answers.map { (k,v) => (k.toString,v) }))
    val pointsRaw = result.bracketAccess("points")
    val points = Python.typeOf(pointsRaw) match {
      case "<class 'fractions.Fraction'>" =>
        Points(Python.asBigInt(pointsRaw.numerator), Python.asBigInt(pointsRaw.denominator))
      case "<class 'int'>" =>
        Points(Python.asBigInt(pointsRaw))
      case "<class 'float'>" =>
        Points(pointsRaw.as[Double])
      case typ => throw RuntimeException(s"Python code in grader set points to type \"$typ\"")
    }
    println(points)
    points
  }

  override def renderHtml: String = ""
}
