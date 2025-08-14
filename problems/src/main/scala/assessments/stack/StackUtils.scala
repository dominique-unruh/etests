package assessments.stack

import StackMath.*
import assessments.{MathContext, UserError}
import assessments.stack.SympyExpr.{ErrorTerm, _equalsTrue, function, get_functions, get_symbols, logger, sympy}
import com.typesafe.scalalogging.Logger
import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.{PyQuote, PythonException, SeqConverters}
import utils.{Python, Utils}

import scala.annotation.targetName

final class SympyExpr(val python: py.Dynamic) extends AnyVal {
  override def toString: String = python.toString
  def apply(args: SympyExpr*): SympyExpr = SympyExpr(python(args.map(_.python)*))
  def latex: String = this match
      case ErrorTerm(message) => s"\\text{ERROR: ${Utils.escapeTeX(message)}}"
      case _ => sympy.latex(python).as[String]
  def equalsTrue(): Boolean = _equalsTrue(python).as[Boolean]
  def +(other: SympyExpr): SympyExpr = SympyExpr(python + other.python)
  def -(other: SympyExpr): SympyExpr = SympyExpr(python - other.python)
  def unary_- : SympyExpr = SympyExpr(- python)
  def *(other: SympyExpr): SympyExpr = SympyExpr(python * other.python)
  def /(other: SympyExpr): SympyExpr = SympyExpr(python / other.python)
  def %(other: SympyExpr): SympyExpr = SympyExpr(python % other.python)
  def **(other: SympyExpr): SympyExpr = SympyExpr(python.__pow__(other.python))
  def gcd(other: SympyExpr): SympyExpr = {
//    println(s"this: $python")
//    println(s"that: ${other.python}")
//    println(s"gcd: ${SympyExpr.gcd(python, other.python)}")
    SympyExpr(SympyExpr.gcd(python, other.python))
  }
  def sqrt: SympyExpr = SympyExpr(sympy.sqrt(python))
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
   * Specifically, every occurrence of `f(x1,...,xn)` is replaced by the result of evaluating `replacement(Seq(x1,...,xn))`.
   *
   * @param name Name of the function f
   * @param replacement Function taking the arguments of f and returning the replacement term
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
  /** Like [[replaceFunctionSeq]], but replaces only occurrences of `f` with no arguments. */
  def replaceFunction(name: String, replacement: () => SympyExpr): SympyExpr =
    replaceFunctionSeq(name, {
      case Seq() => replacement()
      case args => function(name)(args*)
    })
  /** Like [[replaceFunctionSeq]], but replaces only occurrences of `f` with exactly 1 argument. */
  def replaceFunction(name: String, replacement: SympyExpr => SympyExpr): SympyExpr =
    replaceFunctionSeq(name, {
      case Seq(x) => replacement(x)
      case args => function(name)(args*)
    })
  /** Like [[replaceFunctionSeq]], but replaces only occurrences of `f` with exactly 2 arguments. */
  def replaceFunction(name: String, replacement: (SympyExpr,SympyExpr) => SympyExpr): SympyExpr =
    replaceFunctionSeq(name, {
      case Seq(x, y) => replacement(x, y)
      case args => function(name)(args*)
    })

  /** All symbols occurring in the expression. Does not include functions, nor predefined constants like pi. */
  def symbols: Set[String] = {
    get_symbols(python).as[Seq[String]].toSet
  }

  /** All functions occurring in the expression. Does not include predefined functions like Add, Mod. */
  def functions: Set[String] = {
    get_functions(python).as[Seq[String]].toSet
  }

  def names: Set[String] = symbols ++ functions
}

object SympyExpr {
  private val logger = Logger[SympyExpr]
  lazy val sympy: py.Module = py.Module("sympy")

  private val get_symbols = Python.defineFunction("get_symbols",
    "def get_symbols(e): import sympy; return list(map(lambda x: x.name, e.atoms(sympy.Symbol)))")
  private val get_functions = Python.defineFunction("get_functions",
    "def get_functions(e): import sympy; return list(map(lambda f: f.func.name, filter(lambda f: isinstance(f.func,sympy.core.function.UndefinedFunction), e.atoms(sympy.Function))))")

  /** Replacement of sympy.gcd which does not work correctly with variables (e.g., `sympy.gcd(x,3) == 1`). */
  lazy val gcd: py.Dynamic = Python.defineFunction("gcd", """
import sympy
class gcd(sympy.Function):
    @classmethod
    def eval(cls, x, y):
        import sympy
        if x.is_number and y.is_number and x.is_finite and y.is_finite:
            return sympy.gcd(x, y)

        if x == 0:
            return abs(y)
        if y == 0:
            return abs(x)
        if x == y:
            return abs(x)

        return None

    def _eval_simplify(self, **kwargs):
        x, y = self.args
        if x.is_number and y.is_number and x.is_finite and y.is_finite:
            return gcd(x, y)
        return self  # Stay unevaluated
""")

  private lazy val _equalsTrue = py"lambda x: x==True"
  def symbol(name: String): SympyExpr = SympyExpr(sympy.Symbol(name))
  def function(name: String): SympyExpr = SympyExpr(sympy.Function(name))
  def integer(int: Int): SympyExpr = SympyExpr(sympy.Integer(int))
  def fraction(numerator: Int, denominator: Int): SympyExpr =
    assert(denominator != 0)
    integer(numerator) / integer(denominator)

  def Eq(a: SympyExpr, b: SympyExpr) = SympyExpr(sympy.Eq(a.python, b.python))

  given Conversion[Int, SympyExpr] = integer

  def errorTerm(message: String): SympyExpr =
    function("ERROR")(symbol(s"$message"))

  object ErrorTerm {
    private val extract_error = Python.defineFunction("extract_error",
      """def extract_error(term): assert term.func.name == "ERROR"; assert len(term.args) == 1; return term.args[0].name""")

    def unapply(expr: SympyExpr): Option[String] =
      try Some(extract_error(expr.python).as[String])
      catch
        case e : me.shadaj.scalapy.py.PythonException => None
  }

  lazy val `true` = SympyExpr(sympy.S.`true`)
  lazy val `false` = SympyExpr(sympy.S.`false`)
}

object StackUtils {
  // TODO memoize. But this needs first a hashable SympyExpr or something
  def checkEquality(x: SympyExpr, y: SympyExpr, assumption: SympyAssumption = SympyAssumption.positive): Boolean = {
    val result = assumption.addToSympyExpr(SympyExpr.Eq(x, y)).expand.simplify
    result.equalsTrue()
  }

  // TODO: Should be an interable, lazily computed
  def enumerate[A](variables: Set[String])(f: Map[String, StackMath] => A)(implicit mathContext: MathContext): Seq[A] = {
    val loops = mathContext.variables.toList map { (varName, options) =>
      if (options.fixedValue.nonEmpty)
        (varName, Seq(options.fixedValue.get))
      else if (options.testValues.nonEmpty)
        (varName, options.testValues)
      else
        ???
    }
    val results = Seq.newBuilder[A]
    def iter(loops: List[(String, Seq[StackMath])], map: Map[String, StackMath]): Unit = loops match
      case (name, values) :: rest =>
        for (value <- values)
          iter(rest, map + (name -> value))
      case Nil =>
        results += f(map)
    iter(loops, Map.empty)
    results.result()
  }

  def checkEqualityDebug(x: StackMath, y: StackMath)(using MathContext): Seq[(Map[String, StackMath], Boolean)] = {
    val variables: Set[String] = x.variables ++ y.variables
    enumerate(variables) { subst =>
      val x2 = x.mapVariables(subst).toSympyMC(allowUndefined = false)
      val y2 = y.mapVariables(subst).toSympyMC(allowUndefined = false)
      (subst, checkEquality(x2, y2))
    }
  }

  def checkEqualityNew(x: StackMath, y: StackMath)(using MathContext): Boolean =
    checkEqualityDebug(x, y).forall(_._2)
}
