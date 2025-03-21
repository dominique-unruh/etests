package assessments

import assessments.pageelements.{InputElement, MathPreviewElement, PageElement}
import assessments.stack.StackParser.parse
import assessments.stack.StackUtils.checkEquality
import assessments.stack.SympyExpr

import scala.collection.mutable

object StackDefaults {
  def input(reference: String)(using name: sourcecode.Name): InputElement =
    new InputElement(ElementName(name.value), reference)

  extension (str: String) {
    def sympy: SympyExpr = parse(str).toSympy
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

  def checkEq(x: => PageElement, y: => SympyExpr)
             (using answers: Map[ElementName, String], comments: mutable.Builder[String, Seq[String]]): Boolean =
    try {
      checkEquality(x.sympy, y)
    } catch
      case e : SyntaxError => comments += e.getMessage; false
}
