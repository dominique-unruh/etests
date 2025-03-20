package assessments.stack

import StackMath.*
import assessments.UserError
import assessments.stack.SympyExpr._equalsTrue
import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.PyQuote

final case class SympyExpr(python: py.Dynamic) {
  def latex(): String = StackUtils.sympy.latex(python).as[String]
  def equalsTrue(): Boolean = _equalsTrue(python).as[Boolean]
}

object SympyExpr {
  private lazy val _equalsTrue = py"lambda x: x==True"
}

object StackUtils {
  lazy val sympy: py.Module = py.Module("sympy")
  
  def checkEquality(x: SympyExpr, y: SympyExpr): Boolean = {
    val result = SympyExpr(sympy.Eq(x.python, y.python).expand().simplify())
    result.equalsTrue()
  }
}
