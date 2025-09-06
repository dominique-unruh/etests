package utils

import com.typesafe.scalalogging.Logger
import me.shadaj.scalapy.{PyNone, py}
import me.shadaj.scalapy.py.{PyQuote, SeqConverters, exec}

import scala.collection.mutable
import scala.util.Random

object Python {
  private lazy val exec_local = {
    //    py.exec("def exec_local(code): locals = dict(); exec(code, locals=locals); return locals")
    py.exec("def exec_local(code, locals): exec(code, locals=locals); return locals")
    py.Dynamic.global.exec_local
  }

  def execLocally(code: String, locals: Map[String, py.Any] = Map.empty): py.Dynamic = exec_local(code, locals)

  /** Defines a Python function / value.
   *
   * @param name The name of the function/value. Must match what comes after the def in `code`
   * @param code The code defining the function. Something like `def functionname(...): ...` or `functionname = ...`
   * @return The defined function. (Callable)
   * */
  @deprecated("better use define")
  def defineFunction(name: String, code: String): py.Dynamic = {
    execLocally(code).bracketAccess(name)
  }

  private val logger = Logger[Python.type]

  /** Defines a Python value.
   *
   * Example:
   * {{{
   * from sympy import eye
   * I = eye(2)
   * return I
   * }}}
   *
   * Example:
   * {{{
   * from sympy import eye
   * def multI(vec):
   *   return eye(2) * vec
   * return multI
   * }}}
   *
   * @param code The code defining the function. Must end in a return statement that provides the value.
   * @return The defined value
   * */
  def define(code: String): py.Dynamic = {
    val magic = s"magic_${Random.nextInt(1000000000)}"
    val code2 = s"def $magic():\n  ${code.replace("\n", "\n  ")}\n\n"
    //    logger.debug(code2)
    val result = execLocally(code2).bracketAccess(magic)()
    //    logger.debug(result.toString)
    result
  }

  def typeOf(value: py.Dynamic): String =
    py.Dynamic.global.`type`(value).toString

  def asBigInt(value: py.Dynamic): BigInt =
    BigInt(value.toString)

  def varargsWrapper(function: Seq[py.Dynamic] => py.Dynamic): py.Dynamic =
    varargsWrapperPy(function)

  private lazy val varargsWrapperPy = py"lambda function: lambda *args: function(args)"

  lazy val none: py.Dynamic = py.eval("None")

  object List {
    def unapplySeq(value: py.Dynamic): Option[Seq[py.Dynamic]] = {
      if (value == py.None) {
        None
      } else {
        try {
          val pythonType = py.Dynamic.global.`type`(value)
          val typeName = pythonType.`__name__`.as[String]

          if (typeName == "tuple" || typeName == "list") {
            val length = py.Dynamic.global.len(value).as[Int]
            val elements = (0 until length).map(i => value.bracketAccess(i))
            Some(elements)
          } else {
            None
          }
        } catch {
          case _: Exception => None
        }
      }
    }
  }
}