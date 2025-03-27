package utils

import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.{PyQuote, SeqConverters, exec}

import scala.collection.mutable

object Python {
  private lazy val exec_local = {
//    py.exec("def exec_local(code): locals = dict(); exec(code, locals=locals); return locals")
    py.exec("def exec_local(code, locals): exec(code, locals=locals); return locals")
    py.Dynamic.global.exec_local
  }
  def execLocally(code: String, locals: Map[String, py.Any] = Map.empty): py.Dynamic = exec_local(code, locals)

  /** Defines a Python function.
   * 
   * @param name The name of the function. Must match what comes after the def in `code`
   * @param code The code defining the function. Something like `def functionname(...): ...`
   * @return The defined function. (Callable)
   * */
  def defineFunction(name: String, code: String): py.Dynamic = {
    execLocally(code).bracketAccess(name)
  }

  def typeOf(value: py.Dynamic): String =
    py.Dynamic.global.`type`(value).toString
  def asBigInt(value: py.Dynamic): BigInt =
    BigInt(value.toString)
  def varargsWrapper(function: Seq[py.Dynamic] => py.Dynamic): py.Dynamic =
    varargsWrapperPy(function)
  private lazy val varargsWrapperPy = py"lambda function: lambda *args: function(args)"
}
