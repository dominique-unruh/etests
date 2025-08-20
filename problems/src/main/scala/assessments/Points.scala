package assessments

import assessments.Points.bigInt1

import java.math.MathContext
import java.math.RoundingMode
import scala.math.BigDecimal
import scala.util.FromDigits

/** Implementation of rational numbers.
 * 
 * Internal invariant: denominator > 0; numberator and denominator are relatively prime; if numerator=0, then denominator=1 */
class Points private (private val numerator: BigInt, private val denominator: BigInt) {
  assert(denominator > 0)

  def fractionString: String =
    if (denominator == 1)
      numerator.toString
    else
      s"$numerator/$denominator"
  def fractionHtml: String =
    if (denominator == 1)
      numerator.toString
    else
      s"<span><sup>$numerator</sup>&frasl;<sub>$denominator</sub></span>"

  /** Returns the points as a decimal fraction (e.g. 1.23) with high precision (Java defaults) */
  def decimalFractionString: String = (BigDecimal(numerator)/BigDecimal(denominator)).toString
  /** Returns the points as a decimal fraction (e.g. 1.23) with at most `precision` digits after the period. */
  def decimalFractionString(precision: Int): String = {
    val bigdec = (BigDecimal(numerator)/BigDecimal(denominator)).underlying().stripTrailingZeros()
    val bigdec2 = if (bigdec.scale() > precision)
      bigdec.setScale(precision, BigDecimal.RoundingMode.DOWN)
    else if (bigdec.scale() < 0)
      bigdec.setScale(0, BigDecimal.RoundingMode.DOWN)
    else
      bigdec
    bigdec2.toString
  }
  override def toString: String = decimalFractionString(3)

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
  /** Checks whether `string` is a precise representation of this point number.
   * Useful, e.g., to check whether the return value of [[decimalFractionString]] did some rounding or not. */
  def isPreciseString(string: String): Boolean =
    this == Points(BigDecimal(string))

  override def equals(other: Any): Boolean = other match
    case other: Points => (numerator == other.numerator) && (denominator == other.denominator)
}

object Points {
  private val bigInt1 = BigInt(1)
  private val bigInt0 = BigInt(0)
  private val bigInt10 = BigInt(10)
  val one: Points = Points(bigInt1)
  val zero: Points = Points(bigInt0)

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

  def apply(value: Double): Points = Points(value.toString)

  given Conversion[BigInt, Points] with
    def apply(value: BigInt): Points = Points(value)

  given Conversion[Int, Points] with
    def apply(value: Int): Points = Points(value)

  given Conversion[Long, Points] with
    def apply(value: Long): Points = Points(value)

  given Conversion[BigDecimal, Points] with
    def apply(value: BigDecimal): Points = Points(value)

  given Conversion[Double, Points] = value => Points(value)

  given FromDigits.Decimal[Points] with
    override def fromDigits(digits: String): Points =
      Points(digits)

  //  given Ordering[Points] = Ordering.fromLessThan((a,b) => a <= b)
  given Numeric[Points] = new Numeric[Points] {
    override def plus(x: Points, y: Points): Points = x + y

    override def minus(x: Points, y: Points): Points = ???

    override def times(x: Points, y: Points): Points = ???

    override def negate(x: Points): Points = ???

    override def fromInt(x: Int): Points = Points(x)

    override def parseString(str: String): Option[Points] = ???

    override def toInt(x: Points): Int = ???

    override def toLong(x: Points): Long = ???

    override def toFloat(x: Points): Float = ???

    override def toDouble(x: Points): Double = ???

    override def compare(x: Points, y: Points): Int =
      if (x == y) 0
      else if (x < y) -1
      else +1
  }
}