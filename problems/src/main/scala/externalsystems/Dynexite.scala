package externalsystems

import assessments.{Assessment, ElementName, Exam, ExceptionContext, ExceptionWithContext, MarkdownAssessment, Points}
import externalsystems.Dynexite.ResultInputFieldKey
import upickle.core.AbortException

import java.nio.file.{Files, Path}
import upickle.default as up
import utils.Tag

import java.math.MathContext
import scala.annotation.experimental

object Dynexite {
  lazy val resultJsonPath: Path = {
    val path = System.getProperty("dynexite.results.json")
    if (path == null)
      throw RuntimeException("No Dynexite result path configured. Set dynexite.results.json=... in java.properties")
    val path2 = Path.of(path)
    if (!Files.exists(path2))
      throw RuntimeException(s"Dynexite result path points to nonexisting file $path2. Set dynexite.results.json=... in java.properties")
    path2
  }
  lazy val theResults: ExamResults = {
    parseExamResults(resultJsonPath)
  }
  lazy val resultsByLearner: Map[String, Option[Attempt]] =
    theResults.learners.map(learner => learner.identifier -> learner.attempts.lastOption).toMap

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
  final case class Learner(learnerId: String,
                           /** Registration number */ identifier: String,
                           attempts: List[Attempt]) derives up.Reader

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
    case BlockType.`single-choice` => SingleChoiceBlock.fromRaw(raw)
    case BlockType.`result-input` => ResultInputsBlock.fromRaw(raw)
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
      assert(raw.fields.isEmpty)
      val answers =
        raw.answers.get.asInstanceOf[ujson.Arr].value.toSeq
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

  final case class SingleChoiceBlock(answers: Option[Int],
                                     `type`: BlockType,
                                     _comment: Option[String] = None,
                                     maxPointsOpen: Points,
                                     maxPointsClosed: Points,
                                     earnedPointsOpen: Points,
                                     earnedPointsClosed: Points) extends Block
  object SingleChoiceBlock {
    private[Dynexite] def fromRaw(raw: BlockRaw) = {
      assert(raw.fields.isEmpty)
      val answers = raw.answers match {
        case None => None
        case Some(answersRaw) =>
          val answersDouble = answersRaw.asInstanceOf[ujson.Num].value
          assert(answersDouble.isValidInt)
          Some(answersDouble.toInt)
      }

      SingleChoiceBlock(answers = answers, `type` = raw.`type`, _comment = raw._comment,
        maxPointsOpen = raw.maxPointsOpen, maxPointsClosed = raw.maxPointsClosed,
        earnedPointsOpen = raw.earnedPointsOpen, earnedPointsClosed = raw.earnedPointsClosed)
    }
  }

  final case class ResultInputsBlock(answers: Seq[ResultInputsBlockAnswer],
                                     `type`: BlockType,
                                     _comment: Option[String] = None,
                                     maxPointsOpen: Points,
                                     maxPointsClosed: Points,
                                     earnedPointsOpen: Points,
                                     earnedPointsClosed: Points) extends Block

  object ResultInputsBlock {
    private[Dynexite] def fromRaw(raw: BlockRaw) = {
      assert(raw.answers.isEmpty)
      val answers = for (field <- raw.fields.get)
        yield ResultInputsBlockAnswer(answers=field(ResultInputFieldKey.answers), typ=field(ResultInputFieldKey.`type`))

      ResultInputsBlock(answers = answers, `type` = raw.`type`, _comment = raw._comment,
        maxPointsOpen = raw.maxPointsOpen, maxPointsClosed = raw.maxPointsClosed,
        earnedPointsOpen = raw.earnedPointsOpen, earnedPointsClosed = raw.earnedPointsClosed)
    }
  }
  final case class ResultInputsBlockAnswer(answers: String, typ: String)

  final case class StackBlock(answers: Map[String, String],
                              `type`: BlockType,
                              _comment: Option[String] = None,
                              maxPointsOpen: Points,
                              maxPointsClosed: Points,
                              earnedPointsOpen: Points,
                              earnedPointsClosed: Points) extends Block
  object StackBlock {
    private[Dynexite] def fromRaw(raw: BlockRaw) = {
      assert(raw.fields.isEmpty)
      val answers = raw.answers.get.asInstanceOf[ujson.Obj].value.to(Map)
        .map((id,ans) => (id,ans.asInstanceOf[ujson.Str].value))
      StackBlock(answers = answers, `type` = raw.`type`, _comment = raw._comment,
        maxPointsOpen = raw.maxPointsOpen, maxPointsClosed = raw.maxPointsClosed,
        earnedPointsOpen = raw.earnedPointsOpen, earnedPointsClosed = raw.earnedPointsClosed)
    }
  }

  @upickle.implicits.allowUnknownKeys(false)
  final case class BlockRaw(answers: Option[ujson.Value] = None,
                            `type`: BlockType,
                            _comment: Option[String] = None,
                            fields: Option[Seq[Map[ResultInputFieldKey, String]]] = None,
                            maxPointsOpen: Points,
                            maxPointsClosed: Points,
                            earnedPointsOpen: Points,
                            earnedPointsClosed: Points) derives up.Reader

  enum BlockType derives up.ReadWriter {
    case stack
    case classification
    case `single-choice`
    case `result-input`
  }

  enum ResultInputFieldKey derives up.ReadWriter {
    case answers, `type`
  }

  @upickle.implicits.allowUnknownKeys(false)
  final case class BluePrint(blueprintId: String, name: String) derives up.ReadWriter


  @experimental
  def getDynexiteAnswers(assessment: Assessment,
                         exam: Exam,
                         registrationNumber: String,
                         results: Map[String, Option[Attempt]] = Dynexite.resultsByLearner)
                        (implicit exceptionContext: ExceptionContext):
  Map[ElementName, String] = {
    given ExceptionContext = ExceptionContext.addToExceptionContext(s"Matching Dynexite answers up with our exam implementation for $registrationNumber, ${assessment.name}", registrationNumber, assessment)
    results.get(registrationNumber) match {
      case None => throw ExceptionWithContext(s"No student with registration number $registrationNumber known.")
      case Some(None) => throw ExceptionWithContext(s"Student with registration number $registrationNumber made no attempt.")
      case Some(Some(attempt)) =>
        val assessmentIndex = exam.assessmentIndex(assessment)
        if (assessmentIndex >= attempt.items.length)
          throw ExceptionWithContext("Dynexite has less items than there are problems?!")
        val item = attempt.items(assessmentIndex)
        val assessmentDynexiteName = assessment.tags.getOrElse(dynexiteQuestionName, assessment.name)
        if (item.name != assessmentDynexiteName)
          throw ExceptionWithContext(s"Dynexite problem has name ${item.name}, our problem has name $assessmentDynexiteName. Should be equal.")
        val answers = DynexiteGrader.getDynexiteAnswers(item = item, assessment = assessment)
        val reachable = answers.reachable
        if (reachable != assessment.reachablePoints)
          throw ExceptionWithContext(s"Dynexite says there are $reachable reachable points, we say ${assessment.reachablePoints}")
        answers.answers
    }
  }


  object dynexiteQuestionName extends Tag[Assessment, String](default = "")
}
