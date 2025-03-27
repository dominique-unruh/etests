package assessments.stack

import assessments.UserError
import assessments.stack.StackMath.{Bool, Funcall, Integer, Operation, Ops, Variable}
import assessments.stack.SympyExpr.sympy
import me.shadaj.scalapy.py

sealed trait StackMath {
  def mapIdentifiers(f: String => String): StackMath = this match {
    case Operation(operator, arguments@_*) => Operation(operator, arguments.map(_.mapIdentifiers(f))*)
    case Funcall(name, arguments@_*) => Funcall(f(name), arguments.map(_.mapIdentifiers(f))*)
    case Variable(name) => Variable(f(name))
    case Integer(int) => this
    case Bool(bool) => this
  }

  def fixUnderscoreInt: StackMath = mapIdentifiers {
    case StackMath.UnderscoreIntRegex(prefix, suffix) => s"${prefix}_${suffix}"
    case name => name
  }

  def fix: StackMath = fixUnderscoreInt

  def toSympy: SympyExpr = {
    def toSympy(stack: StackMath): py.Dynamic = stack match {
      case Funcall("mod", x, y) => toSympy(x).__mod__(toSympy(y))
      case Funcall("sqrt", x) => sympy.sqrt(toSympy(x))
      case Funcall(name, arguments*) =>
        sympy.Function(name).apply(arguments.map(toSympy) *).as[py.Dynamic]
      case Operation(Ops.power, x, y) => toSympy(x).__pow__(toSympy(y))
      case Operation(Ops.equal, x, y) => sympy.Eq(toSympy(x), toSympy(y)).as[py.Dynamic]
      case Operation(Ops.plus, x, y) => toSympy(x) + toSympy(y)
      case Operation(Ops.minus, x, y) => toSympy(x) - toSympy(y)
      case Operation(Ops.times, x, y) => toSympy(x) * toSympy(y)
      case Operation(Ops.divide, x, y) => toSympy(x) / toSympy(y)
      case Operation(name, arguments*) => throw UserError(s"Unsupported operation $name with ${arguments.length} arguments")
      case Variable(name) => sympy.Symbol(name).as[py.Dynamic]
      case Integer(int) => sympy.Integer(int.toString).as[py.Dynamic]
    }

    SympyExpr(toSympy(this.fix))
  }
}

object StackMath {
  private val UnderscoreIntRegex = "(.*[^0-9_])([0-9]+)".r

  enum Ops {
    case and, not, or, xor
    case equal, less_eq, greater_eq, less, greater
    case power
    case plus, minus
    case times, divide
  }

  case class Operation(operator: Ops, arguments: StackMath*) extends StackMath
  case class Funcall(name: String, arguments: StackMath*) extends StackMath
  case class Variable(name: String) extends StackMath
  case class Integer(int: BigInt) extends StackMath
  case class Bool(bool: Boolean) extends StackMath
}
