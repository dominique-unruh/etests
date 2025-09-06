package assessments.stack

import assessments.{MathContext, UserError}
import assessments.stack.StackMath.{Bool, Funcall, Integer, Operation, Ops, Sympy, Variable, addToStringBuilderCommaSep}
import assessments.stack.SympyExpr.sympy
import me.shadaj.scalapy.py

sealed trait StackMath {
  def variables: Set[String] = {
    val builder = Set.newBuilder[String]
    def collect(math: StackMath): Unit = math match
      case Operation(operator, arguments*) => arguments.foreach(collect)
      case Funcall(name, arguments*) => arguments.foreach(collect)
      case Sympy(op, arguments*) => arguments.foreach(collect)
      case Variable(name) => builder += name
      case Integer(int) =>
      case Bool(bool) =>
    collect(this)
    builder.result()
  }

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
  }

  def mapIdentifiers(f: String => String): StackMath = this match {
    case Operation(operator, arguments*) => Operation(operator, arguments.map(_.mapIdentifiers(f))*)
    case Funcall(name, arguments*) => Funcall(f(name), arguments.map(_.mapIdentifiers(f))*)
    case Sympy(op, arguments*) => Sympy(op, arguments.map(_.mapIdentifiers(f))*)
    case Variable(name) => Variable(f(name))
    case Integer(int) => this
    case Bool(bool) => this
  }


  def mapVariables(f: String => Option[StackMath]): StackMath = this match
    case Operation(operator, arguments*) => Operation(operator, arguments.map(_.mapVariables(f))*)
    case Sympy(op, arguments*) => Sympy(op, arguments.map(_.mapVariables(f))*)
    case Funcall(name, arguments*) => Funcall(name, arguments.map(_.mapVariables(f))*)
    case Variable(name) => f(name).getOrElse(this)
    case Integer(int) => this
    case Bool(bool) => this

  def mapVariables(map: Map[String, StackMath]): StackMath = mapVariables(map.get)

  def mapVariables(subst: (String, StackMath)*): StackMath = mapVariables(Map(subst*))

  /** Applied preprocessors and then fixed values (according to [[MathContext]] */
  def fixValues(using mathContext: MathContext): StackMath = {
    val fixed1 = mathContext.preprocessors.foldLeft(this)((math, preprocessor) => preprocessor(math))
    val fixed2 =
      fixed1.mapVariables { name =>
        for (options <- mathContext.variables.get(name);
             fixedValue <- options.fixedValue)
        yield fixedValue
      }
    fixed2
  }

  def fixUnderscoreInt: StackMath = mapIdentifiers {
    case StackMath.UnderscoreIntRegex(prefix, suffix) => s"${prefix}_${suffix}"
    case name => name
  }

  def fix: StackMath = fixUnderscoreInt

  def toSympyMC(allowUndefined: Boolean = false,
                allowUndefinedFunctions: Boolean = false)(using mathContext: MathContext): SympyExpr = {
    def to(math: StackMath): SympyExpr = math match
      case Operation(operator, arguments@_*) =>
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

    to(this.fixValues)
  }

  @deprecated("Will transition to toSympyMC")
  def toSympy: SympyExpr = {
    def toSympy(stack: StackMath): py.Dynamic = stack match {
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

  def mapFunction(name: String | Ops, f: Seq[StackMath] => StackMath): StackMath = this match
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
    case Variable(name) => this
    case Integer(int) => this
    case Bool(bool) => this
}

object StackMath {
  given Conversion[Int, StackMath] = int => Integer(BigInt(int))

  given Conversion[Long, StackMath] = int => Integer(BigInt(int))

  given Conversion[BigInt, StackMath] = int => Integer(int)


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

  case class Operation(operator: Ops, arguments: StackMath*) extends StackMath
  case class Funcall(name: String, arguments: StackMath*) extends StackMath
  case class Variable(name: String) extends StackMath
  case class Integer(int: BigInt) extends StackMath
  case class Bool(bool: Boolean) extends StackMath
  case class Sympy(op: SympyOperator, arguments: StackMath*) extends StackMath

  class SympyOperator(val name: String, val function: MathContext ?=> Seq[SympyExpr] => SympyExpr) {
    override def toString: String = name
    def apply(arguments: StackMath*): Sympy = Sympy(this, arguments*)
  }

  def addToStringBuilderCommaSep(builder: StringBuilder, items: IterableOnce[StackMath]): Unit = {
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