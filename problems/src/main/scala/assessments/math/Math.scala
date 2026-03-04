package assessments.math

import assessments.math.Math.{Bool, Foreign, Funcall, Integer, Operation, Ops, Sympy, Variable, addToStringBuilderCommaSep}
import assessments.stack.SympyExpr.sympy
import assessments.stack.SympyExpr
import assessments.{ExceptionContext, ExceptionWithContext, MathContext, UserError}
import me.shadaj.scalapy.py
import utils.TypeChecker

import scala.util.boundary
import scala.util.boundary.break

sealed trait Math {
  def cos: Math = Funcall("cos", this)
  def /(other: Math): Math = Operation(Ops.divide, this, other)
  
  def variables: Set[String] = {
    val builder = Set.newBuilder[String]
    def collect(math: Math): Unit = math match
      case Operation(operator, arguments*) => arguments.foreach(collect)
      case Funcall(name, arguments*) => arguments.foreach(collect)
      case Sympy(op, arguments*) => arguments.foreach(collect)
      case Variable(name) => builder += name
      case Integer(_) | Bool(_) | Foreign(_) =>
    collect(this)
    builder.result()
  }

  def +(other: Math): Operation = Operation(Ops.plus, this, other)
  def *(other: Math): Operation = Operation(Ops.times, this, other)
  
  override def toString: String = {
    val builder = new StringBuilder
    addToStringBuilder(builder)
    builder.result()
  }

  def addToStringBuilder(builder: StringBuilder): Unit = this match {
    case Operation(operator, arguments*) =>
      builder ++= operator.toString += '('
      addToStringBuilderCommaSep(builder, arguments)
      builder += ')'
    case Funcall(name, arguments*) =>
      builder ++= name += '('
      addToStringBuilderCommaSep(builder, arguments)
      builder += ')'
    case Sympy(operator, arguments*) =>
      builder ++= "<py:" ++= operator.toString ++= ">("
      addToStringBuilderCommaSep(builder, arguments)
      builder += ')'
    case Variable(name) => builder ++= name
    case Integer(int) => builder ++= int.toString
    case Bool(bool) => builder ++= bool.toString
    case Foreign(value) =>
      builder ++= "[" ++= value.toString ++= "]"
  }

  def mapIdentifiers(f: String => String): Math = this match {
    case Operation(operator, arguments*) => Operation(operator, arguments.map(_.mapIdentifiers(f))*)
    case Funcall(name, arguments*) => Funcall(f(name), arguments.map(_.mapIdentifiers(f))*)
    case Sympy(op, arguments*) => Sympy(op, arguments.map(_.mapIdentifiers(f))*)
    case Variable(name) => Variable(f(name))
    case Integer(_) | Bool(_) | Foreign(_) => this
  }

  def mapVariables(f: String => Option[Math]): Math = this match
    case Operation(operator, arguments*) => Operation(operator, arguments.map(_.mapVariables(f))*)
    case Sympy(op, arguments*) => Sympy(op, arguments.map(_.mapVariables(f))*)
    case Funcall(name, arguments*) => Funcall(name, arguments.map(_.mapVariables(f))*)
    case Variable(name) => f(name).getOrElse(this)
    case Integer(_) | Bool(_) | Foreign(_) => this

  def mapVariables(map: Map[String, Math]): Math = mapVariables(map.get)

  def mapVariables(subst: (String, Math)*): Math = mapVariables(Map(subst*))

  /** Applied preprocessors and then fixed values (according to [[MathContext]] */
  def fixValues(using mathContext: MathContext): Math = {
    val fixed1 = mathContext.preprocessors.foldLeft(this)((math, preprocessor) => preprocessor(math))
    val fixed2 =
      fixed1.mapVariables { name =>
        for (options <- mathContext.variables.get(name);
             fixedValue <- options.fixedValue)
        yield fixedValue
      }
    fixed2
  }

  /** Substitutes the variables for which test cases are set in the [[MathContext]] via `fixVar` or `testValues`.
   * However, not all test values are used, instead the first of the configured test values are used for each specific variable.
   * */
  def someTestValues(using mathContext: MathContext): Math = {
    val fixed1 = fixValues
    val fixed2 = fixed1.mapVariables { name =>
      for (options <- mathContext.variables.get(name);
           testValuesHead <- options.testValues.headOption)
      yield testValuesHead
    }
    fixed2
  }

  def fixUnderscoreInt: Math = mapIdentifiers {
    case Math.UnderscoreIntRegex(prefix, suffix) => s"${prefix}_${suffix}"
    case name => name
  }

  def fix: Math = fixUnderscoreInt

  @deprecated
  def toSympyMC(allowUndefined: Boolean = false,
                allowUndefinedFunctions: Boolean = false)(using mathContext: MathContext): SympyExpr = {
    def to(math: Math): SympyExpr = math match
      case Operation(operator, arguments*) =>
        mathContext.sympyFunctions.get(operator) match
          case Some(function) => function.lift(arguments.map(to)) match
            case Some(value) => value
            case None => throw UndefinedVariableException(s"Operator $operator (for these arguments) in term $this", operator.toString)
          case None => throw UndefinedVariableException(s"Undefined operator $operator in term $this", operator.toString)
      case Funcall(name, arguments*) =>
        def verbatim = SympyExpr(sympy.Function(name).apply(arguments.map(x => to(x).python) *).as[py.Dynamic])
        mathContext.sympyFunctions.get(name) match
          case Some(f) => f.lift(arguments.map(to)) match
            case Some(result) => result
            case None =>
              if (allowUndefinedFunctions) verbatim
              else throw UndefinedVariableException(s"Undefined function $name (for these arguments) in term $this", name)
          case None =>
            if (allowUndefinedFunctions) verbatim
            else throw UndefinedVariableException(s"Undefined function $name in term $this", name)
      case Variable(name) =>
        if (allowUndefined)
          SympyExpr(sympy.Symbol(name).as[py.Dynamic])
        else
          throw UndefinedVariableException(s"Undefined variable $name in term $this", name)
      case Integer(int) => SympyExpr(sympy.Integer(int.toString).as[py.Dynamic])
      case Bool(true) => SympyExpr.`true`
      case Bool(false) => SympyExpr.`false`
      case Sympy(op, arguments*) => op.function(arguments.map(to))
      case Foreign(sympy: SympyExpr) => sympy
      case Foreign(value) =>
        throw RuntimeException(s"Encountered Foreign($value) in .toSympyMC. Not supported.")

    to(this.fixValues)
  }

  @deprecated("Will transition to toSympyMC")
  def toSympy: SympyExpr = {
    def toSympy(stack: Math): py.Dynamic = stack match {
      case Funcall("mod", x, y) => toSympy(x).__mod__(toSympy(y))
      case Funcall("gcd", x, y) => SympyExpr.gcd(toSympy(x), toSympy(y))
      case Funcall("sqrt", x) => sympy.sqrt(toSympy(x))
      case Funcall(name, arguments*) =>
        sympy.Function(name).apply(arguments.map(toSympy) *).as[py.Dynamic]
      case Operation(Ops.power, x, y) => toSympy(x).__pow__(toSympy(y))
      case Operation(Ops.equal, x, y) => sympy.Eq(toSympy(x), toSympy(y)).as[py.Dynamic]
      case Operation(Ops.plus, x, y) => toSympy(x) + toSympy(y)
      case Operation(Ops.minus, x, y) => toSympy(x) - toSympy(y)
      case Operation(Ops.times, x, y) => toSympy(x) * toSympy(y)
      case Operation(Ops.divide, x, y) => toSympy(x) / toSympy(y)
      case Operation(Ops.unaryMinus, x) => - toSympy(x)
      case Operation(Ops.unaryPlus, x) => toSympy(x)
      case Operation(name, arguments*) => throw UserError(s"Unsupported operation $name with ${arguments.length} arguments")
      case Variable(name) => sympy.Symbol(name).as[py.Dynamic]
      case Integer(int) => sympy.Integer(int.toString).as[py.Dynamic]
      case missing => throw RuntimeException(s".toSympy does not support ${missing.getClass} objects. Use .toSympyMC")
    }

    SympyExpr(toSympy(this.fix))
  }

  def mapFunction(name: String | Ops, f: Seq[Math] => Math): Math = this match
    case Operation(operator, arguments*) if operator == name =>
      f(arguments)
    case Operation(operator, arguments*) =>
      Operation(operator, arguments.map(_.mapFunction(name, f))*)
    case Sympy(op, arguments*) =>
      Sympy(op, arguments.map(_.mapFunction(name, f))*)
    case Funcall(fname, arguments*) if fname == name =>
      val res = f(arguments)
      res
    case Funcall(fname, arguments*) =>
      Funcall(fname, arguments.map(_.mapFunction(name, f))*)
    case Variable(_) | Integer(_) | Bool(_) | Foreign(_) => this

  def eval[A](using exceptionContext: ExceptionContext, mathContext: MathContext, typeChecker: TypeChecker[A])
             (debug: Boolean = false): A = {
    given ExceptionContext = ExceptionContext.addToExceptionContext(s"Evaluating formula $this", this)

    def applyFunction(name: String | Math.Ops, arguments: Seq[Math]): Any = {
      val functions = mathContext.functions(name)
      val args = arguments.map(e)
      boundary[Any] {
        for (f <- functions)
          f.lift(args) match {
            case None =>
            case Some(value) =>
              break(value)
          }
        throw ExceptionWithContext(s"Operator or function applied to wrong number of arguments or wrong types")
      }
    }

    def e(math: Math): Any = {
      val result = math match {
        case Math.Funcall(name, arguments*) if mathContext.functions.contains(name) =>
          applyFunction(name, arguments)
        case Math.Operation(Ops.equal, x, y) =>
          e(x) == e(y) // TODO make configurable
        case Math.Operation(operator, arguments*) =>
          throw ExceptionWithContext(s"Unknown operator $operator")
        case Math.Funcall(name, arguments*) =>
          throw ExceptionWithContext(s"Unknown function $name")
        case Math.Variable(name) =>
          throw ExceptionWithContext(s"Encountered variable $name")
        case Math.Integer(int) => int // TODO configurable
        case Math.Bool(bool) => bool // TODO configurable
        case Math.Sympy(op, arguments*) =>
          throw ExceptionWithContext(s"Encountered sympy operation $op")
        case Math.Foreign(value) => value
      }
      if (debug)
        println(s"Eval: $math -> $result")
      result
    }


    e(fixValues) match {
      case typeChecker(result) => result
      case value =>
        throw ExceptionWithContext(s"Formula did not evaluate to a ${typeChecker.name} value but to $value", value)
    }
  }

  def hasSubterm(predicate: Math): Boolean =
    hasSubterm(t => t == predicate)
  
  def hasSubterm(predicate: Math => Boolean): Boolean = {
    def has(math: Math): Boolean = {
      if (predicate(math))
        true
      else math match {
        case Operation(operator, arguments*) => arguments.exists(has)
        case Funcall(name, arguments*) => arguments.exists(has)
        case Variable(name) => false
        case Integer(int) => false
        case Bool(bool) => false
        case Sympy(op, arguments*) => arguments.exists(has)
        case Math.Foreign(value) => false
      }
    }

    has(this)
  }
}

object Math {
  given Conversion[Int, Math] = int => Integer(BigInt(int))

  given Conversion[Long, Math] = int => Integer(BigInt(int))

  given Conversion[BigInt, Math] = int => Integer(int)


  private val UnderscoreIntRegex = "(.*[^0-9_])([0-9]+)".r

  enum Ops {
    case and, not, or, xor
    case equal, less_eq, greater_eq, less, greater
    case power
    case plus, minus
    case times, divide
    case unaryPlus, unaryMinus
    case imaginaryUnit, eulerConstant, pi
    case list, matrix
    /** Special symbol to denote a missing answer */
    case noAnswer
  }

  case class Operation(operator: Ops, arguments: Math*) extends Math
  case class Funcall(name: String, arguments: Math*) extends Math
  case class Variable(name: String) extends Math
  case class Integer(int: BigInt) extends Math
  case class Bool(bool: Boolean) extends Math
  @deprecated
  case class Sympy(op: SympyOperator, arguments: Math*) extends Math
  case class Foreign(value: Any) extends Math

  class SympyOperator(val name: String, val function: MathContext ?=> Seq[SympyExpr] => SympyExpr) {
    override def toString: String = name
    def apply(arguments: Math*): Sympy = Sympy(this, arguments*)
  }

  object SympyOperator {
    def fromPython(name: String, numArgs: Int, pythonFunction: py.Dynamic): SympyOperator = {
      def function(args: Seq[SympyExpr]) =
        if (args.length == numArgs || numArgs == -1)
          SympyExpr(pythonFunction(args.map(_.python)*))
        else
          throw RuntimeException(s"Sympy operator $name called with ${args.length}≠$numArgs arguments")
      new SympyOperator(name, function)
    }
  }

  def addToStringBuilderCommaSep(builder: StringBuilder, items: IterableOnce[Math]): Unit = {
    val iterator = items.iterator
    if (iterator.hasNext) {
      iterator.next().addToStringBuilder(builder)
      for (item <- iterator) {
        builder ++= ", "
        item.addToStringBuilder(builder)
      }
    }
  }

  val imaginaryUnit: Operation = Operation(Ops.imaginaryUnit)
  val eulerConstant: Operation = Operation(Ops.eulerConstant)
  val pi: Operation = Operation(Ops.pi)
  val noAnswer: Operation = Operation(Ops.noAnswer)
}

case class UndefinedVariableException(message: String, varname: String) extends Exception(message)