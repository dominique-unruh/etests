package assessments

import assessments.Points.PointsWrapper

import scala.compiletime.uninitialized

class PointSplitter(total: Points, granularity: Points = 1) {
  if (!(total / granularity).isInteger)
    throw ArithmeticException("Total must be a multiple of granularity")

  private val pointFieldsBuilder = Seq.newBuilder[P]
  private var frozen = false
  private lazy val pointFields: Seq[P] = {
    frozen = true
    val fields = pointFieldsBuilder.result()
    val weights = fields.map(_.weight)
    val totalWeight = weights.sum
    assert(totalWeight > 0)
    // How many units of granularity is the ideal for each field
    val desired = weights.map(_ / totalWeight * total / granularity)
    val desiredTotal = total / granularity
    assert(desiredTotal == desired.sum, (desiredTotal, desired.sum))
    assert(desiredTotal.isInteger)
    val roundedDown: Seq[Points] = desired.map(d => if (d < 1) 1 else d.toBigInt)
    // How much is missing in total
    val missing = desiredTotal - roundedDown.sum
    if (missing < 0)
      throw ArithmeticException(s"PointSplitter, weights=$weights, total=$total, granularity=$granularity: can't solve this without rounding by more than 1 granularity or rounding to 0")
    assert(missing >= 0) // Might not be the case if rounding up small values already took too much
    assert(missing.isInteger)
    // For each field, how much would that field like to be rounded up?
    val missingFractions = desired.zip(roundedDown).map((d,r) => d-r)
    // These will be increased by 1
    val winners = missingFractions.zipWithIndex.sortBy(- _._1).take(missing.toIntExact).map(_._2).toSet
    // Final distribution, still in multiples of granularity
    val rounded = roundedDown.zipWithIndex.map((r,i) => if (winners contains i) r+1 else r)
    assert(rounded.forall(_.isInteger))
    assert(rounded.sum == desiredTotal)
    val pointDistribution = rounded.map(_ * granularity)
    assert(pointDistribution.sum == total)
    assert(fields.length == pointDistribution.length)
    for ((field, points) <- fields.zip(pointDistribution))
      field.points = points
    fields
  }

  class P private [PointSplitter] (private [PointSplitter] val weight: Points, val name: String) extends PointsWrapper {
    assert(!frozen)
    assert(weight >= 0)
    private[PointSplitter] var points: Points = uninitialized
    override def get: Points = { pointFields; points }
    pointFieldsBuilder += this
  }

  /** Initialize a points-field with weight `weight` */
  def w(weight: Points)(using name: sourcecode.Name): P = new P(weight=weight, name=name.value)
  /** Initialize a points-field with weight 1 */
  def p(using name: sourcecode.Name) = P(weight=1, name=name.value)

  def showPoints(): Unit = {
    val maxLen = pointFields.map(_.name.length).max
    println(s"Total points = $total, total weights = ${pointFields.map(_.weight).sum}")
    for (field <- pointFields)
      println(s"${field.name}:${"_" * (maxLen - field.name.length)} ${field}")
  }
}
