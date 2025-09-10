package assessments.stack

import StackMath.*
import assessments.{MathContext, UserError}
import assessments.stack.SympyExpr.{ErrorTerm, _equalsTrue, function, get_functions, get_symbols, logger, sympy, sympyPhysicsQuantum}
import com.typesafe.scalalogging.Logger
import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.{PyQuote, PythonException, SeqConverters}
import utils.{Python, Utils}

import scala.annotation.targetName

final class SympyExpr(val python: py.Dynamic) extends AnyVal {
  override def toString: String = python.toString

  def apply(args: SympyExpr*): SympyExpr = SympyExpr(python(args.map(_.python) *))

  def latex: String = this match
    case ErrorTerm(message) => s"\\text{ERROR: ${Utils.escapeTeX(message)}}"
    case _ => sympy.latex(python).as[String]

  def equalsTrue: Boolean = _equalsTrue(python).as[Boolean]

  def +(other: SympyExpr): SympyExpr = SympyExpr(python + other.python)

  def -(other: SympyExpr): SympyExpr = SympyExpr(python - other.python)

  def unary_- : SympyExpr = SympyExpr(-python)

  def *(other: SympyExpr): SympyExpr = SympyExpr(python * other.python)

  def /(other: SympyExpr): SympyExpr = SympyExpr(python / other.python)

  def %(other: SympyExpr): SympyExpr = SympyExpr(python % other.python)

  def **(other: SympyExpr): SympyExpr = SympyExpr(python.__pow__(other.python))

  @targetName("less")
  def <(other: SympyExpr): Boolean = SympyExpr(python.__lt__(other.python)).equalsTrue
  @targetName("greater")
  def >(other: SympyExpr): Boolean = SympyExpr(python.__gt__(other.python)).equalsTrue
  def <=(other: SympyExpr): Boolean = SympyExpr(python.__le__(other.python)).equalsTrue
  def >=(other: SympyExpr): Boolean = SympyExpr(python.__ge__(other.python)).equalsTrue

  def gcd(other: SympyExpr): SympyExpr = SympyExpr(SympyExpr.gcd(python, other.python))

  def cos: SympyExpr = SympyExpr(sympy.cos(python))
  def sin: SympyExpr = SympyExpr(sympy.sin(python))
  def tan: SympyExpr = SympyExpr(sympy.tan(python))
  
  def abs: SympyExpr = SympyExpr(sympy.Abs(python))
  def sqrt: SympyExpr = SympyExpr(sympy.sqrt(python))
  def round: SympyExpr = SympyExpr(py.Dynamic.global.round(python))
  
  
  def tensor(other: SympyExpr): SympyExpr = 
    SympyExpr(sympyPhysicsQuantum.tensorproduct.TensorProduct(python, other.python))
  
  def isInteger: Boolean =
    SympyExpr.is_integer(python).as[Boolean]

  def substitute(map: (SympyExpr, SympyExpr)*): SympyExpr = {
    val mapPython = map.map((k, v) => (k.python, v.python)).toPythonCopy
    val result = python.subs(mapPython)
    //    println((python, mapPython, result))
    SympyExpr(result)
  }

  /** A xor implemented as addition mod 2.
   * Different from sympy.logic.boolalg.Xor which works on Booleans only
   * (`Xor(2,3)` would be interpreted as `XOR(True,True)`) */
  def xorBit(other: SympyExpr) = SympyExpr(SympyExpr.xorBit(python, other.python))
  /** See [[xorBit]] */
  def andBit(other: SympyExpr) = SympyExpr(SympyExpr.andBit(python, other.python))
  /** See [[xorBit]] */
  def orBit(other: SympyExpr) = SympyExpr(SympyExpr.orBit(python, other.python))

  @targetName("substituteString")
  def substitute(map: (String, SympyExpr)*): SympyExpr =
    substitute(map.map { (k, v) => (SympyExpr.symbol(k), v) } *)

  def expand: SympyExpr = SympyExpr(python.expand())

  def simplify: SympyExpr = SympyExpr(python.simplify())

  /** Replaces all invocations of a function f.
   *
   * Specifically, every occurrence of `f(x1,...,xn)` is replaced by the result of evaluating `replacement(Seq(x1,...,xn))`.
   *
   * @param name        Name of the function f
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
      case args => function(name)(args *)
    })

  /** Like [[replaceFunctionSeq]], but replaces only occurrences of `f` with exactly 1 argument. */
  def replaceFunction(name: String, replacement: SympyExpr => SympyExpr): SympyExpr =
    replaceFunctionSeq(name, {
      case Seq(x) => replacement(x)
      case args => function(name)(args *)
    })

  /** Like [[replaceFunctionSeq]], but replaces only occurrences of `f` with exactly 2 arguments. */
  def replaceFunction(name: String, replacement: (SympyExpr, SympyExpr) => SympyExpr): SympyExpr =
    replaceFunctionSeq(name, {
      case Seq(x, y) => replacement(x, y)
      case args => function(name)(args *)
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

  def !=~(other: SympyExpr) = !algebraicEqual(other)
  def =~(other: SympyExpr) = algebraicEqual(other)
  def algebraicEqual(other: SympyExpr, assumption: SympyAssumption = SympyAssumption.positive): Boolean =
    val result = assumption.addToSympyExpr(SympyExpr.Eq(this, other)).expand.simplify
    result.equalsTrue
}

object SympyExpr {

  def apply(python: py.Dynamic) = new SympyExpr(python)
  def apply(int: Int): SympyExpr = integer(int) 
    
  private val logger = Logger[SympyExpr]
  lazy val sympy: py.Module = py.Module("sympy")
  lazy val sympyPhysicsQuantum: py.Module = py.Module("sympy.physics.quantum")

  private val get_symbols = Python.defineFunction("get_symbols",
    "def get_symbols(e): import sympy; return list(map(lambda x: x.name, e.atoms(sympy.Symbol)))")
  private val get_functions = Python.defineFunction("get_functions",
    "def get_functions(e): import sympy; return list(map(lambda f: f.func.name, filter(lambda f: isinstance(f.func,sympy.core.function.UndefinedFunction), e.atoms(sympy.Function))))")

  /** Replacement of sympy.gcd which does not work correctly with variables (e.g., `sympy.gcd(x,3) == 1`). */
  lazy val gcd: py.Dynamic = Python.defineFunction("gcd",
    """
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
        case e: me.shadaj.scalapy.py.PythonException => None
  }

  lazy val `true`: SympyExpr = SympyExpr(sympy.S.`true`)
  lazy val `false`: SympyExpr = SympyExpr(sympy.S.`false`)
  lazy val zero: SympyExpr = integer(0)
  lazy val imaginaryUnit: SympyExpr = SympyExpr(sympy.S.ImaginaryUnit)
  lazy val eulerConstant: SympyExpr = SympyExpr(sympy.S.Exp1)
  lazy val pi: SympyExpr = SympyExpr(sympy.S.Pi)
  lazy val xorBit: py.Dynamic = Python.define("def xorBit(a,b): from sympy import Integer; return (a+b)%Integer(2)\nreturn xorBit")
  lazy val andBit: py.Dynamic = Python.define("def xorBit(a,b): from sympy import Integer; return (a*b)%Integer(2)\nreturn xorBit")
  lazy val orBit: py.Dynamic = Python.define("def orBit(a,b): from sympy import Integer; return max(a%Integer(2),b%Integer(2))\nreturn orBit")
  def array(components: SympyExpr*): SympyExpr = SympyExpr(sympy.Array(components.map(_.python).toPythonProxy))
  def matrix(rows: SympyExpr*): SympyExpr = SympyExpr(sympy.Matrix(rows.map(_.python).toPythonProxy))
  /** Converts a string into a SympyExpr using the sympy-parser.
   * The sympy parser is unsafe for unsanitized inputs.
   * So we only allow fromString on string literals. */
  def fromString(string : String & Singleton): SympyExpr = fromStringUnsafe(string)
  /** Like [[fromString]] but you need to make sure it's not using on unsafe data */
  def fromStringUnsafe(string : String): SympyExpr = SympyExpr(sympy.sympify(string : String))
  private [SympyExpr] lazy val is_integer = Python.define(
    """import sympy, numbers
      |def is_integer(value):
      |     if isinstance(value, numbers.Number) and not isinstance(value, bool):
      |         value = sympy.Number(value)
      |     if isinstance(value, sympy.Basic):
      |         return value.is_integer
      |     return False
      |return is_integer
      |""".stripMargin)
}