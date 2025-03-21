package assessments.stack

import StackMath.*
import assessments.UserError
import assessments.stack.SympyExpr._equalsTrue
import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.PyQuote
import me.shadaj.scalapy.py.SeqConverters

import scala.annotation.targetName

final case class SympyExpr(python: py.Dynamic) {
  def latex(): String = StackUtils.sympy.latex(python).as[String]
  def equalsTrue(): Boolean = _equalsTrue(python).as[Boolean]
  def +(other: SympyExpr): SympyExpr = SympyExpr(python + other.python)
  def substitute(map: (SympyExpr, SympyExpr)*): SympyExpr = {
    val mapPython = map.map((k,v) => (k.python, v.python)).toPythonCopy
    println(mapPython)
    println(python.subs)
    val result = python.subs(mapPython)
    println(result)
    SympyExpr(result)
  }
  @targetName("substituteString")
  def substitute(map: (String, SympyExpr)*): SympyExpr =
    substitute(map.map { (k,v) => (SympyExpr.symbol(k),v) }*)
}

object SympyExpr {
  private lazy val _equalsTrue = py"lambda x: x==True"
  def symbol(name: String): SympyExpr = SympyExpr(StackUtils.sympy.Symbol(name))
}

object StackUtils {
  lazy val sympy: py.Module = py.Module("sympy")
  
  def checkEquality(x: SympyExpr, y: SympyExpr): Boolean = {
    val result = SympyExpr(sympy.posify(sympy.Eq(x.python, y.python)).bracketAccess(0).expand().simplify())
    result.equalsTrue()
  }
}
