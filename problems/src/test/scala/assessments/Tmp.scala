package assessments

import assessments.stack.{StackMath, StackParser, StackUtils}
import assessments.stack.StackMath.{Funcall, Integer, Operation, Ops, Variable}
import assessments.stack.StackUtils.sympy
import me.shadaj.scalapy
import me.shadaj.scalapy.py

object Tmp {

  val pyGlobal: scalapy.py.Dynamic.global.type = py.Dynamic.global

  def main(args: Array[String]): Unit = {
    val target1 = "mod(k_0^e, N)=c"
    val try1 = "c = mod(k0^e, N) "

    val math = StackParser.parse(target1)
    println(math)
    val tryMath = StackParser.parse(try1)
    val sympyMath = math.toSympy
    val trySympyMath = tryMath.toSympy
    println(sympyMath)
    val latex = sympyMath.latex()
    val tryLatex = trySympyMath.latex()
    println(latex)
    println(tryLatex)

    val check = StackUtils.checkEquality(sympyMath, trySympyMath)
    println(check)
  }
}
