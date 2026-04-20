package assessments.stack

import assessments.math.Math
import assessments.math.Math.*
import assessments.{ExceptionContext, MathContext, UserError}
import assessments.stack.SympyExpr.{ErrorTerm, _equalsTrue, function, get_functions, get_symbols, logger, sympy}
import com.typesafe.scalalogging.Logger
import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.{PyQuote, PythonException, SeqConverters}
import utils.{Python, Utils}

import scala.annotation.targetName
import scala.collection.IterableOnce


object StackUtils {
  @deprecated("Use checkEqualityNew instead")
  def checkEquality(x: SympyExpr, y: SympyExpr, assumption: SympyAssumption = SympyAssumption.positive): Boolean =
    x.algebraicEqual(y, assumption)

  /** For every assignment of test values to the variables, call f.
   * @return Sequences of all the return values of f (lazy evaluation) */
  def enumerateLazy[A](variables: Set[String])(f: Map[String, Math] => A)(using mathContext: MathContext): Iterable[A] = {
    val loops = mathContext.variables.toList map { (varName, options) =>
      if (options.fixedValue.nonEmpty)
        (varName, Seq(options.fixedValue.get))
      else if (options.testValues.nonEmpty)
        (varName, options.testValues)
      else
        ???
    }
    var iterable: Iterable[Map[String, Math]] = Iterable(Map.empty)
    for ((name, testValues) <- loops)
      iterable = for (map <- iterable; value <- testValues) yield map + (name -> value)

    iterable.map(f)
  }

  // TODO: Use enumerateLazy instead?
  /** For every assignment of test values to the variables, call f.
   * @return Sequences of all the return values of f */
  @deprecated("Use enumerateLazy")
  def enumerate[A](variables: Set[String])(f: Map[String, Math] => A)(using mathContext: MathContext): Seq[A] = {
    val loops = mathContext.variables.toList map { (varName, options) =>
      if (options.fixedValue.nonEmpty)
        (varName, Seq(options.fixedValue.get))
      else if (options.testValues.nonEmpty)
        (varName, options.testValues)
      else
        ???
    }
    val results = Seq.newBuilder[A]
    def iter(loops: List[(String, Seq[Math])], map: Map[String, Math]): Unit = loops match
      case (name, values) :: rest =>
        for (value <- values)
          iter(rest, map + (name -> value))
      case Nil =>
        results += f(map)
    iter(loops, Map.empty)
    results.result()
  }
  /** For every assignment of test values to the variables, call f.
   * @return True if all calls to f returned true */
  def forall(variables: Set[String])(f: Map[String, Math] => Boolean)(using mathContext: MathContext): Boolean =
    enumerateLazy(variables)(f).forall(identity)
  /** For every assignment of test values to all variables occurring in terms, call f.
   * f is called with the assignment and with the terms after substituting the assignment.
   * @return Sequences of all the return values of f
   * */
  def enumerateMappedLazy[A](terms: Seq[Math])(f: (Map[String, Math], Seq[Math]) => A)(using mathContext: MathContext): Iterable[A] =
    enumerateLazy(terms.flatMap(_.variables).toSet) { map =>
      val termsMapped = terms.map(_.mapVariables(map))
      f(map, termsMapped)
    }
  /** For every assignment of test values to all variables occurring in terms, call f.
   * f is called with the assignment and with the terms after substituting the assignment.
   * @return Sequences of all the return values of f
   * */
  @deprecated("Use enumerateMappedLazy")
  def enumerateMapped[A](terms: Seq[Math])(f: (Map[String, Math], Seq[Math]) => A)(using mathContext: MathContext): Seq[A] =
    // TODO: Use enumerateLazy instead and return an Iterable?
    enumerate(terms.flatMap(_.variables).toSet) { map =>
      val termsMapped = terms.map(_.mapVariables(map))
      f(map, termsMapped)
    }
  @deprecated("Use enumerateMappedLazy")
  def enumerateMapped[A](x: Math, y: Math)(f: Map[String, Math] => (Math, Math) => A)(using mathContext: MathContext): Seq[A] =
    enumerateMapped(Seq(x,y)) { case (map, Seq(x,y)) => f(map)(x,y) }
  def enumerateMappedLazy[A](x: Math, y: Math)(f: Map[String, Math] => (Math, Math) => A)(using mathContext: MathContext): Iterable[A] =
    enumerateMappedLazy(Seq(x,y)) { case (map, Seq(x,y)) => f(map)(x,y) }
  def enumerateMappedLazy[A](x: Math)(f: Map[String, Math] => Math => A)(using mathContext: MathContext): Iterable[A] =
    enumerateMappedLazy(Seq(x)) { case (map, Seq(x)) => f(map)(x) }

  /** Like [[enumerateMapped]] but returns whether all f-calls return true. */
  def forallMapped(terms: Seq[Math])(f: (Map[String, Math], Seq[Math]) => Boolean)(using mathContext: MathContext): Boolean =
    enumerateMappedLazy(terms)(f).forall(identity)
  /** Like the other `forallMapped` but specifically for two terms */
  def forallMapped(x: Math, y: Math)(f: Map[String, Math] => (Math, Math) => Boolean)(using mathContext: MathContext): Boolean =
    forallMapped(Seq(x,y)){ case (map, Seq(x,y)) => f(map)(x,y) }

  /** Like [[enumerateMapped]] but counts how many f-calls return true.
   * @return (true,total) true=number of true calls, total=total number of calls */
  def countMapped(terms: Seq[Math])(f: (Map[String, Math], Seq[Math]) => Boolean)(using mathContext: MathContext): (Int, Int) = {
    val booleans = enumerateMapped(terms)(f)
    val trues = booleans.count(identity)
    (trues, booleans.length)
  }
  /** Like the other `countMapped` but specifically for two terms */
  def countMapped(x: Math, y: Math)(f: Map[String, Math] => (Math, Math) => Boolean)(using mathContext: MathContext): (Int, Int) =
    countMapped(Seq(x,y)){ case (map, Seq(x,y)) => f(map)(x,y) }


  @deprecated
  def checkEqualityDebug(x: Math, y: Math,
                         mapLeft: SympyExpr => SympyExpr = identity,
                         mapRight: SympyExpr => SympyExpr = identity,
                        )(using MathContext): Seq[(Map[String, Math], SympyExpr, SympyExpr, Boolean)] = {
    val variables: Set[String] = x.variables ++ y.variables
    enumerate(variables) { subst =>
      val x2 = mapLeft(x.mapVariables(subst).toSympyMC(allowUndefined = false))
      val y2 = mapRight(y.mapVariables(subst).toSympyMC(allowUndefined = false))
      (subst, x2, y2, x2.algebraicEqual(y2))
    }
  }

  /** Checks equality `value == expected`.
   *
   * If `value` and `expected` are number-expressions (i.e., contain no variables), the check is done by Sympy.
   *
   * If they contain variables, the equality check is done for every combination of test values configured in the [[MathContext]]
   * using [[MathContext.testValues]] or [[MathContext.fixVar]].
   *
   * If variables occur for which there are no test values, an exception is thrown.
   *
   * @param mapLeft This function is applied to `value` before testing (but after substituting test values)
   * @param mapRight This function is applied to `expected` before testing (but after substituting test values)
   * */
  @deprecated
  def checkEqualityNew(value: Math, expected: Math,
                       mapLeft: SympyExpr => SympyExpr = identity,
                       mapRight: SympyExpr => SympyExpr = identity)(using MathContext): Boolean =
    if (value == Math.noAnswer || expected == Math.noAnswer)
      value == expected
    else
      forallMapped(value.fixValues, expected.fixValues) { _ =>(x, y) =>
        mapLeft(x.toSympyMC(allowUndefined = false))
          .algebraicEqual(mapRight(y.toSympyMC(allowUndefined = false)))
      }

  /** Checks equality `value == expected`.
   *
   * If `value` and `expected` contain no variables, the check is done by running [[Math.eval]] on
   * both and performing an equality check on the resulting rhs and lhs.
   *
   * If they contain variables, the equality check is done for every combination of test values configured in the [[MathContext]]
   * using [[MathContext.testValues]] or [[MathContext.fixVar]].
   *
   * If variables occur for which there are no test values, an exception is thrown.
   **/
  def testEquality(value: Math, expected: Math, equality: (Any,Any) => Boolean)
                  (using MathContext, ExceptionContext): Boolean = {
    forallMapped(value, expected) { _ => (x, y) => equality(x.eval[Any], y.eval[Any]) }
  }
}
