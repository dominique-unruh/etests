package assessments.stack

import StackMath.*
import assessments.UserError
import assessments.stack.SympyExpr.{_equalsTrue, function, sympy}
import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.{PyQuote, PythonException, SeqConverters}
import utils.Python

import scala.annotation.targetName
import scala.runtime.FunctionXXL

final case class SympyExpr(python: py.Dynamic) {
  def apply(args: SympyExpr*): SympyExpr = SympyExpr(python(args.map(_.python)*))
  def latex(): String = sympy.latex(python).as[String]
  def equalsTrue(): Boolean = _equalsTrue(python).as[Boolean]
  def +(other: SympyExpr): SympyExpr = SympyExpr(python + other.python)
  def -(other: SympyExpr): SympyExpr = SympyExpr(python - other.python)
  def %(other: SympyExpr): SympyExpr = SympyExpr(python % other.python)
  def substitute(map: (SympyExpr, SympyExpr)*): SympyExpr = {
    val mapPython = map.map((k,v) => (k.python, v.python)).toPythonCopy
    val result = python.subs(mapPython)
//    println((python, mapPython, result))
    SympyExpr(result)
  }
  @targetName("substituteString")
  def substitute(map: (String, SympyExpr)*): SympyExpr =
    substitute(map.map { (k,v) => (SympyExpr.symbol(k),v) }*)

  def expand: SympyExpr = SympyExpr(python.expand())
  def simplify: SympyExpr = SympyExpr(python.simplify())

  /** Replaces all invocations of a function f.
   *
   * @param name Name of the function f
   * @param replacement TODO
   * */
  def replaceFunctionSeq(name: String, replacement: Seq[SympyExpr] => SympyExpr): SympyExpr = {
    def repl(args: Seq[py.Dynamic]): py.Dynamic = replacement(args.map(p => new SympyExpr(p))).python
//    def repl(args: py.Dynamic): py.Dynamic = replacement(Seq(new SympyExpr(args))).python
//    def repl(x: py.Dynamic, y: py.Dynamic): py.Dynamic = replacement(Seq(new SympyExpr(x), new SympyExpr(y))).python
    try
      SympyExpr(python.replace(sympy.Function(name), Python.varargsWrapper(repl)))
    catch
      case e: PythonException =>
        throw new RuntimeException(s"Python error when replacing $name in $this: ${e.getMessage}")
  }
  def replaceFunction(name: String, replacement: () => SympyExpr): SympyExpr =
    replaceFunctionSeq(name, {
      case Seq() => replacement()
      case args => function(name)(args*)
    })
  def replaceFunction(name: String, replacement: SympyExpr => SympyExpr): SympyExpr =
    replaceFunctionSeq(name, {
      case Seq(x) => replacement(x)
      case args => function(name)(args*)
    })
  def replaceFunction(name: String, replacement: (SympyExpr,SympyExpr) => SympyExpr): SympyExpr =
    replaceFunctionSeq(name, {
      case Seq(x, y) => replacement(x, y)
      case args => function(name)(args*)
    })

  /** All symbols occurring in the expression. Does not include functions, nor predefined constants like pi. */
  def symbols: Set[String] = {
    // TODO as static variable
    val get_symbols = Python.defineFunction("get_symbols",
      "def get_symbols(e): import sympy; return list(map(lambda x: x.name, e.atoms(sympy.Symbol)))")
    get_symbols(python).as[Seq[String]].toSet
  }

  /** All functions occurring in the expression. Does not include predefined functions like Add, Mod. */
  def functions: Set[String] = {
    // TODO as static variable
    val get_functions = Python.defineFunction("get_functions",
      "def get_functions(e): import sympy; return list(map(lambda f: f.func.name, filter(lambda f: isinstance(f.func,sympy.core.function.UndefinedFunction), e.atoms(sympy.Function))))")
    get_functions(python).as[Seq[String]].toSet
  }

  def names: Set[String] = symbols ++ functions
}

object SympyExpr {
  lazy val sympy: py.Module = py.Module("sympy")

  private lazy val _equalsTrue = py"lambda x: x==True"
  def symbol(name: String): SympyExpr = SympyExpr(sympy.Symbol(name))
  def function(name: String): SympyExpr = SympyExpr(sympy.Function(name))
  def integer(int: Int): SympyExpr = SympyExpr(sympy.Integer(int))

  def Eq(a: SympyExpr, b: SympyExpr) = SympyExpr(sympy.Eq(a.python, b.python))

  given Conversion[Int, SympyExpr] = integer

  def errorTerm(message: String): SympyExpr =
    function("ERROR")(symbol(s"$message"))
}

object StackUtils {
  def checkEquality(x: SympyExpr, y: SympyExpr, assumption: SympyAssumption = SympyAssumption.positive): Boolean = {
    val result = assumption.addToSympyExpr(SympyExpr.Eq(x, y)).expand.simplify
    result.equalsTrue()
  }
}
