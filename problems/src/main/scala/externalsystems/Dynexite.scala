package externalsystems

import scala.language.implicitConversions

import assessments.pageelements.{AnswerElement, MultipleChoice}
import assessments.{Assessment, ElementName, Exam, ExceptionContext, ExceptionWithContext, MarkdownAssessment, Points}
import externalsystems.Dynexite.ResultInputFieldKey
import upickle.core.AbortException

import java.nio.file.{Files, Path}
import upickle.default as up
import utils.{Tag, Utils}

import java.math.MathContext
import java.util.zip.ZipFile
import scala.annotation.experimental
import scala.collection.mutable
import scala.jdk.CollectionConverters.given
import scala.util.Random

object Dynexite {
  private val resultJsonPaths = mutable.Map[String, Path]()
  def resultJsonPath(exam: Exam): Path = resultJsonPaths.getOrElseUpdate(exam.getClass.getName, {
    val property = s"dynexite.results.json.${exam.getClass.getSimpleName.stripSuffix("$")}"
    Utils.getSystemPropertyPath(property, s"JSON file with Dynexite results for exam ${exam.name}")
  })

  private val examResultsMap = mutable.Map[String, ExamResults]()
  def examResults(exam: Exam): ExamResults = examResultsMap.getOrElseUpdate(exam.getClass.getName, {
    parseExamResults(resultJsonPath(exam))
  })
  private val resultsByLearnerMap = mutable.Map[String, Map[String, Option[Attempt]]]()
  def resultsByLearner(exam: Exam): Map[String, Option[Attempt]] = resultsByLearnerMap.getOrElseUpdate(exam.getClass.getName, {
    examResults(exam).learners.map(learner => learner.identifier -> learner.attempts.lastOption).toMap
  })

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

  def getDynexiteAnswersRaw(assessment: Assessment,
                         exam: Exam,
                         registrationNumber: String)
                           (implicit exceptionContext: ExceptionContext) = {
    resultsByLearner(exam).get(registrationNumber) match {
      case None => throw ExceptionWithContext(s"No student with registration number $registrationNumber known.")
      case Some(None) => throw ExceptionWithContext(s"Student with registration number $registrationNumber made no attempt.")
      case Some(Some(attempt)) =>
        val assessmentIndex = exam.assessmentIndex(assessment)
        if (assessmentIndex >= attempt.items.length)
          throw ExceptionWithContext("Dynexite has less items than there are problems?!")
        val item = attempt.items(assessmentIndex)
        item.blocks.map(_.answers)
    }
  }

  def getDynexiteAnswers(assessment: Assessment,
                         exam: Exam,
                         registrationNumber: String)
                        (implicit exceptionContext: ExceptionContext):
  Map[ElementName, String] = {
    given ExceptionContext = ExceptionContext.addToExceptionContext(s"Matching Dynexite answers up with our exam implementation for $registrationNumber, ${assessment.name}", registrationNumber, assessment)
    val (answers, points, reachable) = resultsByLearner(exam).get(registrationNumber) match {
      case None => throw ExceptionWithContext(s"No student with registration number $registrationNumber known.")
      case Some(None) => throw ExceptionWithContext(s"Student with registration number $registrationNumber made no attempt.")
      case Some(Some(attempt)) =>
        val assessmentIndex = exam.assessmentIndex(assessment)
        if (assessmentIndex >= attempt.items.length)
          throw ExceptionWithContext("Dynexite has less items than there are problems?!")
        val item = attempt.items(assessmentIndex)
        val assessmentDynexiteName = assessment.tags.getOrElse(dynexiteQuestionName, assessment.name)
        if (item.name != assessmentDynexiteName)
          throw ExceptionWithContext(s"Dynexite problem has name '${item.name}', our problem has name '$assessmentDynexiteName'. Should be equal.")
        getDynexiteAnswers(item = item, assessment = assessment)
    }
    if (reachable != assessment.reachablePoints)
      throw ExceptionWithContext(s"Dynexite says there are $reachable reachable points, we say ${assessment.reachablePoints}")
    answers
  }

/*  def blockNumberAnswers(block: Block): Int = block match
    case block: StackBlock => block.answers.size
    case block: ClassificationBlock => block.answers.length
    case block: SingleChoiceBlock => 1*/

  def getDynexiteAnswers(item: Dynexite.Item, assessment: Assessment)
                        (implicit exceptionContext: ExceptionContext):
  (Map[ElementName, String], Points, Points) = {
    val blockAssignment = assessment.tags.getOrElse(dynexiteBlockAssignment, {
      if (item.blocks.length > 1)
        throw ExceptionWithContext(s"Problem ${assessment.name} corresponds to ${item.blocks.length} Dynexite input blocks. Please add tag dynexiteBlockAssignment to the problem.")
      Seq(assessment.pageElements.values.collect { case elem: AnswerElement => elem }.toSeq)
    })

    assert(Utils.isDistinct(blockAssignment.flatten))

    assert(blockAssignment.length == item.blocks.length)

    val maps = for ((answerElements, block) <- blockAssignment.zip(item.blocks)) yield { block match
      case block: StackBlock => getDynexiteAnswersStack(block, answerElements, assessment)
      case block: ClassificationBlock => getDynexiteAnswersClassification(block, answerElements)
      case block: SingleChoiceBlock => getDynexiteAnswersSingleChoiceBlock(block, answerElements)
      case block: ResultInputsBlock => getDynexiteAnswersResultInputsBlock(block, answerElements)
    }

  /*  item.blocks match {
      case Seq() => DynexiteResponses(Map.empty, 0, 0)
      case Seq(_: StackBlock, _*) =>
        assert(item.blocks.forall(_.isInstanceOf[StackBlock]))
        getDynexiteAnswersStack(item, assessment)
      case Seq(_: ClassificationBlock, _*) =>
        assert(item.blocks.forall(_.isInstanceOf[ClassificationBlock]))
        getDynexiteAnswersClassification(item, assessment)
      case Seq(block, _*) =>
        throw ExceptionWithContext(s"Dynexite data contained a solution block of unsupported type ${block.getClass.getName}")
    }*/

    val points = item.blocks.map(_.earnedPoints).sum
    val reachable = item.blocks.map(_.maxPoints).sum

    (Map(maps.flatten*), points, reachable)
  }

  private def getDynexiteAnswersResultInputsBlock(block: Dynexite.ResultInputsBlock, elements: Seq[AnswerElement])
                                                 (implicit exceptionContext: ExceptionContext): Map[ElementName, String] = {
    val dynexiteAnswers = block.answers.map(_.answers)

    val assessmentNames = elements.map(_.name)

    assert(dynexiteAnswers.length == assessmentNames.length, (dynexiteAnswers, assessmentNames))

    val answers = mutable.Map[ElementName, String]()

    for ((name, answer) <- assessmentNames.zip(dynexiteAnswers)) {
      assert(!answers.contains(name), (answers, name, assessmentNames))
      answers.update(name, Option(answer).getOrElse(""))
    }

    answers.toMap
  }

  private def getDynexiteAnswersSingleChoiceBlock(block: Dynexite.SingleChoiceBlock, elements: Seq[AnswerElement])
                                                 (implicit exceptionContext: ExceptionContext): Map[ElementName, String] = {
    val Seq(element) = elements
    val answer = block.answers

    if (!element.isInstanceOf[MultipleChoice])
      throw ExceptionWithContext(s"Dynexite content block of type single choice is matched up with input field ${element.name} of type ${element.getClass.getSimpleName}, but must be type MultipleChoice")
    val options = element.asInstanceOf[MultipleChoice].options


    val answerString = answer match
      case Some(int) =>
        if (int < 0)
          throw ExceptionWithContext(s"Dynexite content block of type single choice returned option choice number $int. Only positive numbers allowed!")
        if (int >= options.size)
          throw ExceptionWithContext(s"Dynexite content block of type single choice is matched up with input field ${element.name}. The former selected option #$int (starting from 0), but ${element.name} has only ${options.size} options.")
        options.keys.toSeq(int)
      case None => ""

    Map(element.name -> answerString)
  }

  private def getDynexiteAnswersStack(block: Dynexite.StackBlock, elements: Seq[AnswerElement], assessment: Assessment)
                                     (implicit exceptionContext: ExceptionContext):
  Map[ElementName, String] = {
    var points: Points = 0
    var reachable: Points = 0

    val expectedNames = {
      val builder = mutable.Map[String, ElementName]()
      for (element <- elements) {
        val lastName = element.name.name
        assert(!builder.contains(lastName), (builder, lastName, elements))
        builder.update(lastName, element.name)
      }
      builder.toMap
    }

    val answers = mutable.Map[ElementName, String]()

    val subRegex = "(.*)_sub_([0-9]+)_([0-9]+)".r
    
    for ((name, value) <- block.answers;
         if !subRegex.matches(name)) {
      val elementName = expectedNames.getOrElse(name,
        throw ExceptionWithContext(s"$name (answer name from Dynexite/Stack) not in the list of input fields of ${assessment.name} (${expectedNames.keys.mkString(", ")})",
          name, assessment, expectedNames)
      )
      assert(!answers.contains(elementName))
      answers.update(elementName, value)
    }

    val matrices: mutable.Map[String, mutable.Map[(Int, Int), String]] = mutable.Map()
    for (case (`subRegex`(name, row, col), value) <- block.answers) {
      // Input with name name_sub_row_col
      val matrix = matrices.getOrElseUpdate(name, mutable.Map())
      val r = row.toInt
      val c = col.toInt
      assert(!matrix.contains((r,c)))
      matrix.put((r,c), value)
    }
    
    for ((matrixName, content) <- matrices) {
      assert(!answers.contains(ElementName(matrixName)))
      val maxRow = content.keys.map(_._1).max
      val maxCol = content.keys.map(_._2).max
      val string = StringBuilder()
      for (row <- 0 to maxRow) {
        if (row > 0) string += '\n'
        for (col <- 0 to maxCol) {
          if (col > 0) string += ' '
          val entry = content((row,col))
          val cleanedEntry = entry.replaceAll("\\s", "")
          string ++= cleanedEntry
        }
      }
      answers(ElementName(matrixName)) = string.result()
    }
    
    for (expected <- expectedNames.values
         if !answers.contains(expected))
      answers.put(expected, "")

    answers.toMap
  }

  private def getDynexiteAnswersClassification(block: Dynexite.ClassificationBlock, elements: Seq[AnswerElement])
                                              (implicit exceptionContext: ExceptionContext):
  Map[ElementName, String] = {
    val dynexiteAnswers = block.answers

    val assessmentNames = elements.map(_.name)

    assert(dynexiteAnswers.length == assessmentNames.length, (dynexiteAnswers, assessmentNames))

    val answers = mutable.Map[ElementName, String]()

    for ((name, answer) <- assessmentNames.zip(dynexiteAnswers)) {
      assert(!answers.contains(name), (answers, name, assessmentNames))
      answers.update(name, Option(answer).getOrElse(""))
    }

    answers.toMap
  }

  final case class DynexiteResponses(answers: Map[ElementName, String], points: Points, reachable: Points)

  def getLinkForLearner(exam: Exam, registration: String): String = {
    val attempt = resultsByLearner(exam)(registration).getOrElse {
      throw RuntimeException(s"Cannot generate link to Dynexite exam: Student $registration didn't write it.") }
    val attemptId = attempt.attemptId
    val blueprintId = Dynexite.examResults(exam).blueprint.blueprintId
    val course = exam.tags.getOrElse(dynexiteCourseId, throw RuntimeException(s"Exam $exam has no tag dynexiteCourseId"))
    val url = s"https://dynexite.rwth-aachen.de/t/companies/cpsippjadbec73a3unm0/courses/$course/exams/$blueprintId/grader?attempt=$attemptId&seed="
    // The URL also supports item=... with some item-id (looking like this: d2914rbadbec73f2974g) but taking the item-ids from the Dynexite JSON doesn't work
    url
  }

  val dynexiteQuestionName = Tag[Assessment, String](default = "")
  /** In a Dynexite question that has several input blocks, this tag needs to be set.
   * It assigns the answer elements belonging to each block to that block.
   * Example: You have a Stack question with answers ans1, ans2, and a multiple choice one with mc1, mc2.
   * Then set this to Seq(Seq(ans1,ans2),Seq(mc1,mc2)).
   * For non-stack problems, the order of answer elements matters.
   * Can also be used it there is just one input block; this can make sure the answer elements are in the right order.
   * */
  val dynexiteBlockAssignment = Tag[Assessment, Seq[Seq[AnswerElement]]](default = Seq.empty)
  /** From links like https://dynexite.rwth-aachen.de/t/companies/cpsippjadbec73a3unm0/courses/XXX/exams/ (the XXX part) */
  val dynexiteCourseId = Tag[Exam, String](default = null)

  def getAnswerPDF(exam: Exam,
                   registrationNumber: String): Array[Byte] = {
    val archive: Path = Utils.getSystemPropertyPath(
      s"dynexite.results.pdfs.${exam.getClass.getSimpleName.stripSuffix("$")}", "the Dynexite PDF zip")
    val zip = new ZipFile(archive.toFile)

    var pdf: Array[Byte] = null
    for (entry <- zip.entries().asIterator().asScala) {
//      logger.debug(s"$registrationNumber, $entry")
      if (entry.getName.startsWith(registrationNumber) && entry.getName.endsWith(".pdf")) {
        pdf = zip.getInputStream(entry).readAllBytes()
      }
    }
    assert(pdf != null)
    pdf
  }
  
  def randomLearner(exam: Exam): String = {
    val learners = resultsByLearner(exam).view.collect({ case (regno, Some(_)) => regno }).toVector
    val index = Random.nextInt(learners.length)
    learners(index)
  }
}
