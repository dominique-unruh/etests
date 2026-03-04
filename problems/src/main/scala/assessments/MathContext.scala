package assessments

import assessments.MathContext.VarOptions
import assessments.MathContext
import assessments.math.StackMath
import assessments.math.StackMath.Ops
import assessments.stack.SympyExpr.sympy
import assessments.stack.SympyExpr
import me.shadaj.scalapy.py

import scala.annotation.constructorOnly

case class MathContext private (variables: Map[String, VarOptions],
                                functions: Map[String | StackMath.Ops, Seq[PartialFunction[Seq[Any], Any]]],
                                sympyFunctions: Map[String | StackMath.Ops, PartialFunction[Seq[SympyExpr], SympyExpr]],
                                preprocessors: Seq[StackMath => StackMath],
                               ) {
  @deprecated
  def symbol(name: String, options: VarOptions): MathContext = {
    assert(!variables.contains(name))
    val options2 = options.copy(name = name)
    copy(variables = variables + (name -> options2))
  }

  /** Sets the fixed value of the variable `name` */
  def fixVar(name: String, value: StackMath): MathContext =
    symbol(name, VarOptions(fixedValue = Some(value)))
  def testValues(name: String, values: StackMath*): MathContext =
    symbol(name, VarOptions(testValues = values))

  /** Specifies the behavior of the function or operator `name` by giving an evaluation function. */
  @deprecated
  def sympyFunction(name: String | StackMath.Ops, function: PartialFunction[Seq[SympyExpr], SympyExpr]): MathContext =
    copy(sympyFunctions = sympyFunctions + (name -> function))
  @deprecated
  def sympyFunction(name: String | StackMath.Ops, function: py.Dynamic, argNumber: Int): MathContext = {
    sympyFunction(name, {
      case args if args.length == argNumber => SympyExpr(function(args.map(_.python)*)) 
    })
  }

  @deprecated
  def preprocessor(preprocessor: StackMath => StackMath): MathContext =
    copy(preprocessors = preprocessors.appended(preprocessor))

  @deprecated
  def preprocessor(functionName: String, preprocessor: Seq[StackMath] => StackMath): MathContext =
    this.preprocessor { m => m.mapFunction(functionName, preprocessor) }

  /** Removes the function with name `name` */
  def withoutFunction(name: String | StackMath.Ops): MathContext =
    copy(functions = functions.removed(name))

  /** Adds an interpretation to function `name`.
   *
   * Note: by default, this does not overwrite previous interpretations.
   * Instead, when a function is evaluated (e.g., [[StackMath.eval]]), the result of the first-add interpretation that succeeds is used.
   **/
  def withFunctionPartial(name: String | StackMath.Ops, function: PartialFunction[Seq[Any], Any], overwrite: Boolean = false): MathContext = {
    val existing = if (overwrite) Seq.empty else functions.getOrElse(name, Seq.empty)
    copy(functions = functions.updated(name, existing appended function))
  }

  def withFunction1[X](name: String | StackMath.Ops, function: X => Any, overwrite: Boolean = false): MathContext =
    withFunctionPartial(name, { case Seq(x) => function(x.asInstanceOf[X]) }, overwrite = overwrite)

  def withFunction2[X, Y](name: String | StackMath.Ops, function: (X, Y) => Any, overwrite: Boolean = false): MathContext =
    withFunctionPartial(name, { case Seq(x, y) => function(x.asInstanceOf[X], y.asInstanceOf[Y]) }, overwrite = overwrite)
}

object MathContext {
  private val sympyFunctions = Map[String | StackMath.Ops, PartialFunction[Seq[SympyExpr], SympyExpr]](
    "mod" -> { case Seq(x,y) => x % y },
    "cos" -> { case Seq(x) => x.cos },
    "sin" -> { case Seq(x) => x.sin },
    "tan" -> { case Seq(x) => x.tan },
    "gcd" -> { case Seq(x,y) => x.gcd(y) },
    "sqrt" -> { case Seq(x) => x.sqrt },
    "matrix" -> { case rows => SympyExpr.matrix(rows*) },
    Ops.power -> { case Seq(x,y) => x ** y },
    Ops.plus -> { case Seq(x,y) => x + y },
    Ops.minus -> { case Seq(x,y) => x - y },
    Ops.times -> { case Seq(x,y) => x * y },
    Ops.divide -> { case Seq(x,y) => x / y },
    Ops.unaryPlus -> { case Seq(x) => x },
    Ops.unaryMinus -> { case Seq(x) => - x },
    Ops.equal -> { case Seq(x,y) => SympyExpr(sympy.Eq(x.python, y.python).as[py.Dynamic]) },
    Ops.imaginaryUnit -> { case Seq() => SympyExpr.imaginaryUnit },
    Ops.eulerConstant -> { case Seq() => SympyExpr.eulerConstant },
    Ops.pi -> { case Seq() => SympyExpr.pi },
    Ops.list -> { args => SympyExpr.array(args*) }
  )

  val default = new MathContext(
    variables = Map.empty,
    functions = Map.empty,
    preprocessors = Seq.empty,
    sympyFunctions =  sympyFunctions,
  )
  @deprecated
  case class VarOptions(name: String = "",
                        fixedValue: Option[StackMath] = None,
                        testValues: Seq[StackMath] = Seq.empty) {
    assert(fixedValue.isEmpty || testValues.isEmpty)
  }
}

