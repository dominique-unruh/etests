package assessments

import java.math.MathContext
import scala.util.FromDigits

/** Implementation of rational numbers.
 * 
 * Internal invariant: denominator > 0; numberator and denominator are relatively prime; if numerator=0, then denominator=1 */
class Points private (private val numerator: BigInt, private val denominator: BigInt) {
  assert(denominator > 0)

  def fractionString: String = s"$numerator/$denominator"
  def decimalFractionString: String = (BigDecimal(numerator)/BigDecimal(denominator)).toString
  override def toString: String = decimalFractionString

  def <(other: Points): Boolean =
    numerator * other.denominator < other.numerator * denominator
  def >(other: Points): Boolean = other < this
  def <=(other: Points): Boolean =
    numerator * other.denominator <= other.numerator * denominator
  def >=(other: Points): Boolean = other <= this
  def +(other: Points): Points = Points(
    numerator * other.denominator + other.numerator * denominator,
    denominator * other.denominator)
  def -(other: Points): Points = Points(
    numerator * other.denominator - other.numerator * denominator,
    denominator * other.denominator)
  def *(other: Points): Points = Points(numerator * other.numerator, denominator * other.denominator)
  def /(other: Points): Points = {
    assert(other.denominator != 0)
    Points(numerator * other.denominator, denominator * other.numerator)
  }
  def abs: Points = {
    // Bypassing normalization by direct constructor call because this is already normalized
    new Points(numerator.abs, denominator)
  }

  override def equals(other: Any): Boolean = other match
    case other: Points => (numerator == other.numerator) && (denominator == other.denominator)
}

object Points {
  def apply(numerator: BigInt, denominator: BigInt): Points = {
    var num = numerator
    var den = denominator
    assert(den != 0)
    if (den < 0) {
      num = -num
      den = -den
    }
    if (denominator != 1) {
      val gcd = num.gcd(den)
      if (gcd != 1) {
        num /= gcd
        den /= gcd
      }
    }
    new Points(num, den)
  }
  def apply(value: String): Points = Points(BigDecimal(value, MathContext.UNLIMITED))
  def apply(value: BigInt): Points = new Points(value, bigInt1)
  def apply(value: Int): Points = new Points(value, bigInt1)
  def apply(value: Long): Points = new Points(value, bigInt1)
  def apply(value: BigDecimal): Points =
    Points(value.bigDecimal.unscaledValue(),
      bigInt10.pow(value.bigDecimal.scale()))
  private val bigInt1 = BigInt(1)
  private val bigInt10 = BigInt(10)
  given Conversion[BigInt, Points] with
    def apply(value: BigInt): Points = Points(value)
  given Conversion[Int, Points] with
    def apply(value: Int): Points = Points(value)
  given Conversion[Long, Points] with
    def apply(value: Long): Points = Points(value)
  given Conversion[BigDecimal, Points] with
    def apply(value: BigDecimal): Points = Points(value)
  given FromDigits.Decimal[Points] with
    override def fromDigits(digits: String): Points =
      Points(digits)
}
