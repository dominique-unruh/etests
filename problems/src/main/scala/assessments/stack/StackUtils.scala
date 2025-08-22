package assessments.stack

import StackMath.*
import assessments.{MathContext, UserError}
import assessments.stack.SympyExpr.{ErrorTerm, _equalsTrue, function, get_functions, get_symbols, logger, sympy}
import com.typesafe.scalalogging.Logger
import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.{PyQuote, PythonException, SeqConverters}
import utils.{Python, Utils}

import scala.annotation.targetName





object StackUtils {
  // TODO memoize. But this needs first a hashable SympyExpr or something
  @deprecated
  def checkEquality(x: SympyExpr, y: SympyExpr, assumption: SympyAssumption = SympyAssumption.positive): Boolean =
    x.algebraicEqual(y, assumption)

  // TODO: Should be an interable, lazily computed
  /** For every assignment of test values to the variables, call f.
   * @return Sequences of all the return values of f */
  def enumerate[A](variables: Set[String])(f: Map[String, StackMath] => A)(using mathContext: MathContext): Seq[A] = {
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
  /** For every assignment of test values to the variables, call f.
   * @return True if all calls to f returned true */
  def forall(variables: Set[String])(f: Map[String, StackMath] => Boolean)(using mathContext: MathContext): Boolean =
    enumerate(variables)(f).forall(identity)
  /** For every assignment of test values to all variables occurring in terms, call f.
   * f is called with the assignment and with the terms after substituting the assignment.
   * @return Sequences of all the return values of f
   * */
  def enumerateMapped[A](terms: Seq[StackMath])(f: (Map[String, StackMath], Seq[StackMath]) => A)(using mathContext: MathContext): Seq[A] =
    enumerate(terms.flatMap(_.variables).toSet) { map =>
      val termsMapped = terms.map(_.mapVariables(map))
      f(map, termsMapped)
    }
  def enumerateMapped[A](x: StackMath, y: StackMath)(f: Map[String, StackMath] => (StackMath, StackMath) => A)(using mathContext: MathContext): Seq[A] =
    enumerateMapped(Seq(x,y)) { case (map, Seq(x,y)) => f(map)(x,y) }

  /** Like [[enumerateMapped]] but returns whether all f-calls return true. */
  def forallMapped(terms: Seq[StackMath])(f: (Map[String, StackMath], Seq[StackMath]) => Boolean)(using mathContext: MathContext): Boolean =
    enumerateMapped(terms)(f).forall(identity)
  /** Like the other `forallMapped` but specifically for two terms */
  def forallMapped(x: StackMath, y: StackMath)(f: Map[String, StackMath] => (StackMath, StackMath) => Boolean)(using mathContext: MathContext): Boolean =
    forallMapped(Seq(x,y)){ case (map, Seq(x,y)) => f(map)(x,y) }

  def checkEqualityDebug(x: StackMath, y: StackMath,
                         mapLeft: SympyExpr => SympyExpr = identity,
                         mapRight: SympyExpr => SympyExpr = identity,
                        )(using MathContext): Seq[(Map[String, StackMath], SympyExpr, SympyExpr, Boolean)] = {
    val variables: Set[String] = x.variables ++ y.variables
    enumerate(variables) { subst =>
      val x2 = mapLeft(x.mapVariables(subst).toSympyMC(allowUndefined = false))
      val y2 = mapRight(y.mapVariables(subst).toSympyMC(allowUndefined = false))
      (subst, x2, y2, x2.algebraicEqual(y2))
    }
  }

  def checkEqualityNew(value: StackMath, expected: StackMath,
                       mapLeft: SympyExpr => SympyExpr = identity,
                       mapRight: SympyExpr => SympyExpr = identity)(using MathContext): Boolean =
    if (value == StackMath.noAnswer || expected == StackMath.noAnswer)
      value == expected
    else
      forallMapped(value, expected) { _ =>(x, y) =>
        mapLeft(x.toSympyMC(allowUndefined = false))
          .algebraicEqual(mapRight(y.toSympyMC(allowUndefined = false)))
      }
}
