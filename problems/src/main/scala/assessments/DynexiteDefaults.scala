package assessments

import assessments.pageelements.*
import assessments.stack.StackParser.parse
import assessments.stack.StackUtils.checkEquality
import assessments.stack.{SympyAssumption, SympyExpr}

import scala.collection.mutable

object DynexiteDefaults {
  private def elementName(name: sourcecode.Name) =
    ElementName(name.value.replace('$', '.'))

  def input(reference: String)(using name: sourcecode.Name): InputElement =
    new InputElement(elementName(name), reference)

  def multi(options: Seq[String], reference: String)(using name: sourcecode.Name): MultipleChoice =
    new MultipleChoice(name=elementName(name), options=options, reference=reference)

  extension (str: String) {
    def sympy: SympyExpr = {
      if (str == "" || str == null)
        SympyExpr.errorTerm("empty")
      else try
        parse(str).toSympy
      catch
        case e: SyntaxError => SympyExpr.errorTerm(e.getMessage)
    }
  }

  extension (pe: PageElement) {
    def stringValue(using valueMap: Map[ElementName, String]): String = valueMap.getOrElse(pe.name, "")
    def sympy(using valueMap: Map[ElementName, String]): SympyExpr = stringValue.sympy
  }

  private def stackMathRender(string: String): String =
    if (string == "")
      ""
    else try
      string.sympy.latex()
    catch
      case e: Exception =>
        s"\\text{${e.getMessage}}" // TODO Should be escaped

  def preview(observed: PageElement)(using name: sourcecode.Name) =
    MathPreviewElement(ElementName(name.value), observed.name, stackMathRender)

  def checkEq(x: => PageElement | SympyExpr, y: => PageElement | SympyExpr, assumption: SympyAssumption = SympyAssumption.positive)
             (using answers: Map[ElementName, String], comments: Commenter): Boolean =
    try {
      def toSympy(value: PageElement | SympyExpr) = value match {
        case x: PageElement => x.sympy
        case x: SympyExpr => x
      }
      checkEquality(toSympy(x), toSympy(y), assumption=assumption)
    } catch
      case e : SyntaxError => comments += e.getMessage; false


  def gradeInputGroup(inputs: Seq[(AnswerElement[String], String)],
                      pointsPerOption: Points = null, pointsTotal: Points = null)
                     (using commenter: Commenter, valueMap: Map[ElementName, String]): Points = {
    assert(inputs.nonEmpty)
    assert(pointsPerOption != null || pointsTotal != null)
    if (pointsPerOption != null && pointsTotal != null)
      assert(pointsTotal == pointsPerOption * inputs.length, (pointsTotal, pointsPerOption))
    val pointsPerOption2 = if (pointsPerOption==null) pointsTotal / inputs.length else pointsPerOption
    val pointsTotal2 = if (pointsTotal==null) pointsPerOption * inputs.length else pointsTotal

    var points: Points = 0

    for ((input, description) <- inputs) {
      if (input.stringValue == input.reference)
        commenter += s"$description: Correct."
        points += pointsPerOption2
      else
        commenter += s"$description: Incorrect. (You said ${input.stringValue}, should be ${input.reference})"
    }

    points
  }
}
