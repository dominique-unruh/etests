package externalsystems

import assessments.Points
import upickle.core.AbortException

import java.nio.file.{Files, Path}
import upickle.default as up

import java.math.MathContext

object Dynexite {
  def parseExamResults(path: Path): ExamResults = {
    try
      up.read[ExamResults](path)
    catch
      case e @ AbortException(_, index, _, _, _) =>
        val presnippet = Files.readString(path).substring(index-20, index)
        val snippet = Files.readString(path).substring(index, index+50)
        println(s"ABORT at ...$presnippet■$snippet...")
        throw e
  }

  @upickle.implicits.allowUnknownKeys(false)
  final case class ExamResults(version: Int, blueprint: BluePrint, learners: List[Learner]) derives up.Reader {
    assert(version == 4)
  }

  @upickle.implicits.allowUnknownKeys(false)
  final case class Learner(learnerId: String, identifier: String, attempts: List[Attempt]) derives up.Reader

  @upickle.implicits.allowUnknownKeys(false)
  final case class Attempt(attemptId: String,
                           items: List[Item],
                           maxPointsOpen: Points,
                           maxPointsClosed: Points,
                           earnedPointsOpen: Points,
                           earnedPointsClosed: Points)
    derives up.Reader {
    def maxPoints: Points = maxPointsOpen + maxPointsClosed
    def earnedPoints: Points = earnedPointsOpen + earnedPointsClosed
  }

  @upickle.implicits.allowUnknownKeys(false)
  final case class Item(itemId: String,
                        name: String,
                        blocks: List[Block],
                        maxPointsOpen: Points,
                        maxPointsClosed: Points,
                        earnedPointsOpen: Points,
                        earnedPointsClosed: Points)
    derives up.Reader {
    def maxPoints: Points = maxPointsOpen + maxPointsClosed
    def earnedPoints: Points = earnedPointsOpen + earnedPointsClosed
  }

  sealed trait Block {
    val answers: Any
    val `type`: BlockType
    val _comment: Option[String]
    val maxPointsOpen: Points
    val maxPointsClosed: Points
    val earnedPointsOpen: Points
    val earnedPointsClosed: Points

    def maxPoints: Points = maxPointsOpen + maxPointsClosed
    def earnedPoints: Points = earnedPointsOpen + earnedPointsClosed
  }
  given up.Reader[Block] = up.reader[BlockRaw].map[Block] { raw => raw.`type` match {
    case BlockType.classification => ClassificationBlock.fromRaw(raw)
    case BlockType.stack => StackBlock.fromRaw(raw)
  }}

/*  implicit object PointsReader extends up.SimpleReader[Points] {
    override def expectedMsg = "expected number of points"
    override def visitString(s: CharSequence, index: Int) = Points(s.toString)
    override def visitInt32(d: Int, index: Int): Points = d
    override def visitInt64(d: Long, index: Int) = d
    override def visitUInt64(d: Long, index: Int) = d
    override def visitFloat32(d: Float, index: Int) = ???
    override def visitFloat64(d: Double, index: Int) = ???
    override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = visitString(s, index)
  }*/

  given up.Reader[Points] = up.reader[Double].map[Points](BigDecimal(_, MathContext(16)))
  
  final case class ClassificationBlock(answers: Seq[String],
                                       `type`: BlockType,
                                       _comment: Option[String] = None,
                                       maxPointsOpen: Points,
                                       maxPointsClosed: Points,
                                       earnedPointsOpen: Points,
                                       earnedPointsClosed: Points) extends Block
  object ClassificationBlock {
    private[Dynexite] def fromRaw(raw: BlockRaw) = {
      val answers =
        raw.answers.asInstanceOf[ujson.Arr].value.toSeq
          .map {
            case ujson.Null => null
            case ujson.Str(str) => str
            case v => throw AssertionError((v, v.getClass))
          }
      ClassificationBlock(answers = answers, `type` = raw.`type`, _comment = raw._comment,
        maxPointsOpen = raw.maxPointsOpen, maxPointsClosed = raw.maxPointsClosed,
        earnedPointsOpen = raw.earnedPointsOpen, earnedPointsClosed = raw.earnedPointsClosed)
    }
  }

  final case class StackBlock(answers: Map[String, String],
                              `type`: BlockType,
                              _comment: Option[String] = None,
                              maxPointsOpen: Points,
                              maxPointsClosed: Points,
                              earnedPointsOpen: Points,
                              earnedPointsClosed: Points) extends Block
  object StackBlock {
    private[Dynexite] def fromRaw(raw: BlockRaw) = {
      val answers = raw.answers.asInstanceOf[ujson.Obj].value.to(Map)
        .map((id,ans) => (id,ans.asInstanceOf[ujson.Str].value))
      StackBlock(answers = answers, `type` = raw.`type`, _comment = raw._comment,
        maxPointsOpen = raw.maxPointsOpen, maxPointsClosed = raw.maxPointsClosed,
        earnedPointsOpen = raw.earnedPointsOpen, earnedPointsClosed = raw.earnedPointsClosed)
    }
  }

  @upickle.implicits.allowUnknownKeys(false)
  final case class BlockRaw(answers: ujson.Value,
                            `type`: BlockType,
                            _comment: Option[String] = None,
                            maxPointsOpen: Points,
                            maxPointsClosed: Points,
                            earnedPointsOpen: Points,
                            earnedPointsClosed: Points) derives up.Reader

  enum BlockType derives up.ReadWriter {
    case stack
    case classification
  }

  @upickle.implicits.allowUnknownKeys(false)
  final case class BluePrint(blueprintId: String, name: String) derives up.ReadWriter
}
