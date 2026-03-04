/*
package assessments.math

import assessments.MathContext.MathSystemContext
import assessments.{ExceptionContext, ExceptionWithContext, MathContext}
import assessments.math.MathSystem.Pattern
import assessments.stack.StackMath
import assessments.stack.StackMath.Ops
import spire.math.ULong

trait MathSystem[N] {
  final type Scalar = N
  type Bool = Boolean
  type Type >: Scalar | Bool
  val name: String
  val Scalar: Pattern[Type, Scalar]
  def from(int: BigInt): Scalar
  def from(int: Int): Scalar = from(BigInt(int))
  def from(bool: Boolean): Bool
  def equal(x: Type, y: Type): Bool
}

object MathSystem {
  trait Pattern[T, U] {
    def unapply(arg: T): Option[U]
  }

  extension (math: StackMath) {
    def eval[N](using exceptionContext: ExceptionContext, mathSystem: MathSystem[N], mathContext: MathContext): N = {
      given ExceptionContext = ExceptionContext.addToExceptionContext(s"Evaluating formula $math using ${mathSystem.name}", math, mathSystem)
      given MathSystemContext[N] = mathContext.forMathSystem
      math.evalRaw[N] match {
        case mathSystem.Scalar(scalar) => scalar
        case value =>
          throw ExceptionWithContext(s"Formula did not evaluate to a scalar value but to $value", value)
      }
    }
  }
}

object ULongMathSystem extends MathSystem[ULong] {
  override val name: String = "unsigned longs"
  override type Type = ULong | Boolean
  override object Scalar extends Pattern[Type, ULong] {
    override def unapply(arg: Type): Option[ULong] = arg match {
      case long: ULong => Some(long)
      case _ => None
    }
  }
  override def from(int: BigInt): ULong = ULong.fromBigInt(int)
  override def from(int: Int): ULong = ULong.fromInt(int)
  override def from(bool: Bool): Boolean = bool
  override def equal(x: Type, y: Type): ULongMathSystem.Bool = x == y
}

*/
