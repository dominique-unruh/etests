package assessments.stack

import assessments.stack.SympyExpr.sympy
import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.PyQuote
import utils.Python

import scala.collection.mutable
import scala.util.Random

class SympyAssumption private (private val assumptions: Map[String, Boolean]) {
  def &(other: SympyAssumption) = {
    val newAssumptions = mutable.Map[String, Boolean]()
    newAssumptions.addAll(assumptions)
    for ((k,v) <- other.assumptions)
      newAssumptions.get(k) match {
        case Some(oldValue) if oldValue != v =>
          throw IllegalArgumentException("Contradictory assumptions")
        case _ =>
          newAssumptions.update(k,v)
      }
    new SympyAssumption(newAssumptions.toMap)
  }

  def addToSympyExpr(expr: SympyExpr): SympyExpr =
    SympyExpr(SympyAssumption.addAssumptions(expr.python, assumptions))
}

object SympyAssumption {
  /*

  val x = (py"""(lambda x: str($sympy.Symbol))""")
  println(x)
  println(x(1))
  private val addAssumptions =
    py"""lambda expr, assumptions: \
      expr.replace(lambda x: x.is_symbol, lambda x: ($sympy.Symbol("bla"), x)[1])"""
  private val addAssumptionsTmp =
    py"""lambda expr, assumptions: \
      expr.replace(lambda x: isinstance(x, $sympy.Symbol), lambda x: $sympy.Symbol(x.name, **assumptions)) \
          .replace(lambda f: isinstance(f, $sympy.Function) and isinstance(f.func, $sympy.core.function.UndefinedFunction),
                   lambda f: $sympy.Function(f.func.name, **assumptions)(*f.args))"""
*/
  private val addAssumptions = {
    val code =
      """def add_assumptions(expr, assumptions):
        |    import sympy
        |    return expr.replace(lambda x: isinstance(x, sympy.Symbol), lambda x: sympy.Symbol(x.name, **assumptions)) \
        |               .replace(lambda f: isinstance(f, sympy.Function) and isinstance(f.func,sympy.core.function.UndefinedFunction),
        |                        lambda f: sympy.Function(f.func.name, **assumptions)(*f.args))""".stripMargin
    Python.defineFunction("add_assumptions", code)
  }

  val positive = new SympyAssumption(Map("positive" -> true))
  val integer = new SympyAssumption(Map("integer" -> true))
}
