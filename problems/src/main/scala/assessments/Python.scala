package assessments

import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.SeqConverters

import scala.collection.mutable

object Python {
  private lazy val exec_local = {
//    py.exec("def exec_local(code): locals = dict(); exec(code, locals=locals); return locals")
    py.exec("def exec_local(code, locals): exec(code, locals=locals); return locals")
    py.Dynamic.global.exec_local
  }
  def execLocally(code: String, locals: Map[String, py.Any] = Map.empty): py.Dynamic = exec_local(code, locals)
  def typeOf(value: py.Dynamic): String =
    py.Dynamic.global.`type`(value).toString
  def asBigInt(value: py.Dynamic): BigInt =
    BigInt(value.toString)
}
