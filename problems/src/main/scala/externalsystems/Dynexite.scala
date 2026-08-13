package externalsystems

import scala.language.implicitConversions
import assessments.pageelements.{AnswerElement, InputElement, MultipleChoice}
import assessments.{Assessment, ElementName, Exam, ExceptionContext, ExceptionWithContext, MarkdownAssessment, Points}
import com.typesafe.scalalogging.Logger
import externalsystems.Dynexite.ResultInputFieldKey
import upickle.core.AbortException

import java.nio.file.{Files, Path}
import upickle.default as up
import utils.{Tag, Utils}

import java.math.MathContext
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse, WebSocket, WebSocketHandshakeException}
import java.util.concurrent.{CompletionException, CompletionStage, LinkedBlockingQueue, TimeUnit}
import java.util.zip.ZipFile
import scala.annotation.experimental
import scala.collection.mutable
import scala.jdk.CollectionConverters.given
import scala.util.Random

object Dynexite {
  private val examResultsMap = mutable.Map[String, ExamResults]()
  def examResults(exam: Exam): ExamResults = synchronized {
    examResultsMap.getOrElseUpdate(exam.getClass.getName, {
      val results = exam.tags.getOrElse(dynexiteJSONResults,
        throw RuntimeException(s"Exam ${exam.name} doesn't have tag ${dynexiteJSONResults}"))
      parseExamResults(results)
    })
  }
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

  def getDynexiteAnswersRaw(problem: Assessment,
                            exam: Exam,
                            registrationNumber: String)
                           (implicit exceptionContext: ExceptionContext): Seq[Any] = {
    val item = getDynexiteItem(problem, exam, registrationNumber)
    item.blocks.map(_.answers)
  }

  def getDynexiteItem(problem: Assessment,
                      exam: Exam,
                      registrationNumber: String)
                     (implicit exceptionContext: ExceptionContext): Item = {
    resultsByLearner(exam).get(registrationNumber) match {
      case None => throw ExceptionWithContext(s"No student with registration number $registrationNumber known.")
      case Some(None) => throw ExceptionWithContext(s"Student with registration number $registrationNumber made no attempt.")
      case Some(Some(attempt)) =>
        assert(Utils.isDistinct(attempt.items.map(_.name)))

        val dynexiteProblems = Map.from(attempt.items.map(item => item.name -> item))
        val examProblems = Map.from(exam.problems.map(problem => problem.tags.getOrElse(dynexiteQuestionName, problem.name) -> problem))

        if ((dynexiteProblems.keySet -- examProblems.keySet).nonEmpty)
          throw ExceptionWithContext(s"The following problems exist in Dynexite but not in our exam (fix name or use tag dynexiteQuestionName?): ${(dynexiteProblems.keySet -- examProblems.keySet).mkString(", ")}")
        if ((examProblems.keySet -- dynexiteProblems.keySet).nonEmpty)
          throw ExceptionWithContext(s"The following problems exist in our exam but not in Dynexite: ${(examProblems.keySet -- dynexiteProblems.keySet).mkString(", ")}")
        dynexiteProblems(problem.tags.getOrElse(dynexiteQuestionName, problem.name))
    }
  }

  def getDynexiteAnswers(problem: Assessment,
                         exam: Exam,
                         registrationNumber: String)
                        (implicit exceptionContext: ExceptionContext):
  Map[ElementName, String] = {
    given ExceptionContext = ExceptionContext.addToExceptionContext(s"Matching Dynexite answers up with our exam implementation for $registrationNumber, ${problem.name}", registrationNumber, problem)
    val item = getDynexiteItem(problem, exam, registrationNumber)
    val (answers, points, reachable) = getDynexiteAnswers(item = item, assessment = problem)
    if (reachable != problem.reachablePoints)
      throw ExceptionWithContext(s"Dynexite says there are $reachable reachable points, we say ${problem.reachablePoints}")
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

    val answers = Map.newBuilder[ElementName, String]
    val processed = mutable.Map[String, ElementName]()
    def markProcessed(answerName: String, processingElement: ElementName) = {
      if (processed.contains(answerName))
        throw ExceptionWithContext(s"Answer ${answerName} processed by both ${processed(answerName).name} and ${processingElement.name}")
      processed.put(answerName, processingElement)
    }

    for (element <- elements) {
      val answer = element match {
        case input: InputElement =>
          // TODO: For certain matrices, this will not work properly, see commented code below instead
//          logger.warn(block.answers.toString())
          markProcessed(element.name.name, element.name)
          answers += element.name -> block.answers.getOrElse(element.name.name, "")
        case choice: MultipleChoice => choice.style match {
          case MultipleChoice.Style.select | MultipleChoice.Style.radio =>
            markProcessed(element.name.name, element.name)
            val optionNumber = block.answers.get(element.name.name).map(_.toInt)
            assert(optionNumber.forall(_ > 0))
            val optionName = optionNumber.map(i => choice.options.keySet.toSeq(i-1)).getOrElse("")
            answers += element.name -> optionName
          case MultipleChoice.Style.checkbox =>
//            logger.warn(s"Answers: ${block.answers}")
            val extendedName = element.name.name ++ "_1"
            markProcessed(extendedName, element.name)
            val answer = block.answers.get(extendedName) match {
              case Some("1") => choice.yesAnswer
              case None => choice.noAnswer
              case Some(_) => ??? // Unexpected
            }
            answers += element.name -> answer
        }
      }
    }

    block.answers.get("COMMENT_FIELD") match {
      case None | Some("") =>
        answers += ElementName.extraData -> ""
      case Some(value) =>
        answers += ElementName.extraData -> ("Comment field: " + value)
    }
    markProcessed("COMMENT_FIELD", ElementName.extraData)

    val unprocessed = block.answers.keys.toSet -- processed.keys
    if (unprocessed.nonEmpty)
      throw ExceptionWithContext(s"Unprocessed answers in Dynexite data: ${unprocessed.map(n => s"$n=${block.answers(n)}").mkString(", ")}")

    answers.result()
/*
    val subRegex = "(.*)_sub_([0-9]+)_([0-9]+)".r

    var comment = ""

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
    
 */
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
  val dynexiteCourseId = Tag[Exam, String]()
  /** From links like https://dynexite.rwth-aachen.de/t/companies/cpsippjadbec73a3unm0/courses/.../exams/XXX/development (the XXX part) */
  val dynexiteExamId = Tag[Exam, String]()
  /** From links like https://dynexite.rwth-aachen.de/t/companies/cpsippjadbec73a3unm0/items/XXX/edit (the XXX part) */
  val dynexiteQuestionId = Tag[Assessment, String]()
  /** ZIP file with all student answers as PDFs, as downloaded from Dynexite */
  val dynexitePDFArchive = Tag[Exam, Path]()
  /** JSON file with the exam answers as downloaded from Dynexite */
  val dynexiteJSONResults = Tag[Exam, Path]()

  def getAnswerPDF(exam: Exam,
                   registrationNumber: String): Array[Byte] = {
    val archive: Path = exam.tags.getOrElse(dynexitePDFArchive,
      throw RuntimeException(s"Exam ${exam.name} does not have the tag ${dynexitePDFArchive}"))
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

  /** Path of the file holding the Dynexite auth cookie (temporary, for testing). Kept in a
   * user-private directory; see [[Utils.readOrPromptUserSecret]]. */
  private val authCookieFile = Path.of("/tmp/etests-tmp/dynexite-authcookie")

  /** Name of the Dynexite session cookie. */
  private val cookieName = "dyn-orbit-teacher"

  /** The Dynexite editor URL for a question item (`…/items/<id>/edit`). */
  def editUrl(questionId: String): String =
    s"https://dynexite.rwth-aachen.de/t/companies/cpsippjadbec73a3unm0/items/$questionId/edit"

  def examUrl(courseId: String, examId: String): String =
    s"https://dynexite.rwth-aachen.de/t/companies/cpsippjadbec73a3unm0/courses/$courseId/exams/$examId/development"

  /** Ensure the cookie string carries the `dyn-orbit-teacher=` prefix; if only the value was
   * stored/entered, prepend the name. (The value itself may contain `=` padding, so we key off
   * the prefix, not the presence of any `=`.) */
  private def withCookieName(cookie: String): String =
    if (cookie.startsWith(s"$cookieName=")) cookie else s"$cookieName=$cookie"

  /** Reject anything that isn't a plausible Dynexite item id (the `<id>` in a `…/items/<id>/…`
   * URL), so a stale/wrong `dynexiteQuestionId` fails loudly instead of hitting the server. */
  private def requireItemId(questionId: String): Unit =
    if (!questionId.matches("[A-Za-z0-9]+"))
      throw RuntimeException(s"'$questionId' is not a valid Dynexite item id (expected something " +
        s"like 'd9uad1badbec73djbua0', the <id> in a '…/items/<id>/edit' URL). Fix the " +
        s"dynexiteQuestionId tag of this problem.")

  /** Read the Dynexite auth cookie from [[authCookieFile]], prompting the user if absent. */
  private def readCookie(): String = {
    val cookie = Utils.readOrPromptUserSecret(authCookieFile, "Dynexite auth cookie") {
      Utils.promptForString(
        s"Enter the Dynexite auth cookie value (the '$cookieName' cookie; get it from the browser devtools after login):")
    }
    if (cookie.isEmpty) throw RuntimeException(s"Auth cookie ($authCookieFile) is empty.")
    cookie
  }

  /** Mark a Dynexite question item as reviewed and upload its question XML, sharing one cookie.
   *
   * The review call runs first and doubles as the interactive auth check: it is attempted with the
   * stored cookie ([[authCookieFile]]), and on HTTP 401 the cookie is discarded, a fresh one is
   * prompted, and review is retried once. The now-known-good cookie is then reused for the upload,
   * which is attempted a single time (see [[upload]] for its behaviour / safety checks). */
  def markReviewedAndUpload(questionId: String, xml: String, expectedName: String, title: String,
                            reachablePoints: Points): Unit = {
    requireItemId(questionId)
    val cookie =
      try { val c = readCookie(); markReviewed(questionId, c); c }
      catch case e: DynexiteUnauthenticated =>
        logger.warn(s"Dynexite rejected the auth cookie (expired?): ${e.getMessage}. " +
          s"Discarding $authCookieFile and asking for a new one.")
        Files.deleteIfExists(authCookieFile)
        val c = readCookie(); markReviewed(questionId, c); c
    upload(questionId, xml, expectedName, title, reachablePoints, cookie)
  }

  /** Thrown when Dynexite rejects the auth cookie — a bad/expired/missing/misformatted cookie.
   * Surfaces as an HTTP 401 from the review REST call, or as a WebSocket handshake rejection
   * (HTTP 400/401/403) from the builder upload. */
  private class DynexiteUnauthenticated(message: String, cause: Throwable = null) extends RuntimeException(message, cause)


  /** Mark a Dynexite question item as reviewed (the approval-before-publishing state on the
   * `…/items/<id>/review` page). Two steps, as the UI does: `POST /t/api/v1/items/<id>/review`
   * (empty body) puts the item into "review pending", then `PUT` with `{"isAccepted":true,
   * "message":null}` accepts it.
   *
   * Single attempt, no auth-retry: `cookie` is supplied by the caller. HTTP 401 throws
   * [[DynexiteUnauthenticated]] (so the caller can refresh the cookie). An HTTP 500 whose JSON body
   * has `message == "server-error"` is ignored — Dynexite answers that way when the item is already
   * in the requested review state. */
  private def markReviewed(questionId: String, cookie: String): Unit = {
    val url = URI.create(s"https://dynexite.rwth-aachen.de/t/api/v1/items/$questionId/review")
    val client = HttpClient.newHttpClient()

    def request(method: HttpRequest.Builder => HttpRequest.Builder): Unit = {
      val req = method(HttpRequest.newBuilder(url)
        .header("Cookie", withCookieName(cookie))
        .header("Origin", "https://dynexite.rwth-aachen.de")
        .header("Referer", s"https://dynexite.rwth-aachen.de/t/companies/cpsippjadbec73a3unm0/items/$questionId/review")
        .header("Content-Type", "application/json")
        .header("Accept", "application/json, text/plain, */*")).build()
      val response = client.send(req, HttpResponse.BodyHandlers.ofString())
      val code = response.statusCode()
      if (code >= 200 && code < 300) return
      if (code == 401)
        throw DynexiteUnauthenticated(s"Dynexite returned HTTP 401 for review of item " +
          s"$questionId (${req.method}).")
      // "server-error" on a 500 just means the item is already in that review state; ignore it.
      if (code == 500 && serverErrorBody(response.body())) {
        logger.info(s"Dynexite review step ${req.method} for item $questionId returned " +
          s"server-error (already in that state?); ignoring.")
        return
      }
      throw RuntimeException(s"Dynexite returned HTTP $code marking item $questionId reviewed " +
        s"(${req.method}): ${response.body()}")
    }

    // Step 1: request review (moves the item into "review pending").
    request(_.POST(HttpRequest.BodyPublishers.noBody()))
    // Step 2: accept the review.
    request(_.PUT(HttpRequest.BodyPublishers.ofString(
      ujson.write(ujson.Obj("isAccepted" -> ujson.Bool(true), "message" -> ujson.Null)))))
    logger.info(s"Marked Dynexite item $questionId as reviewed.")
  }

  /** True if `body` is Dynexite's `{"…","message":"server-error"}` JSON (best-effort parse). */
  private def serverErrorBody(body: String): Boolean =
    try ujson.read(body).obj.get("message").map(_.str).contains("server-error")
    catch case _: Throwable => false

  /** Turn Dynexite's `errorMap` (block-uuid -> list of `{reason, message, line, column, ...}`)
   * into a readable multi-line string. Falls back to raw JSON if the shape is unexpected. */
  private def formatValidationErrors(errorMap: ujson.Value): String =
    try
      errorMap.obj.map { (uuid, errors) =>
        val items = errors.arr.map { e =>
          val eo = e.obj
          val reason = eo.get("reason").map(_.str).filter(_.nonEmpty).getOrElse("error")
          val line = eo.get("line").map(_.num.toInt).getOrElse(0)
          val column = eo.get("column").map(_.num.toInt).getOrElse(0)
          val loc = if (line != 0 || column != 0) s" (line $line, column $column)" else ""
          val message = eo.get("message").map(_.str).getOrElse("").trim
          s"  - [$reason]$loc\n" + message.linesIterator.map("      " + _).mkString("\n")
        }.mkString("\n")
        s"Block $uuid:\n$items"
      }.mkString("\n")
    catch case _: Throwable => ujson.write(errorMap, indent = 2)

  /** HTTP status of a handshake rejection, if `t` (or its cause) is one. The status is not always
   * exposed (some rejections surface only as a [[WebSocketHandshakeException]] with no response),
   * so callers should treat any [[WebSocketHandshakeException]] as a handshake failure. */
  private def handshakeStatus(t: Throwable): Option[Int] = t match {
    case h: WebSocketHandshakeException => Option(h.getResponse).map(_.statusCode)
    case _ => None
  }

  /** Upload a Moodle/STACK question XML directly into an existing Dynexite item, via the builder
   * WebSocket (`/t/api/sub/builder/<questionId>`). The item must already exist; this overwrites its
   * (single) block's `questionXML`, `title`, and reachable points (`logic.points`). The server
   * re-parses/re-renders, so only `questionXML` need be supplied.
   *
   * As a safety check, the item's name must equal `expectedName` (guards against a wrong/stale
   * `dynexiteQuestionId`); a mismatch throws and nothing is uploaded. Single attempt: `cookie` is
   * supplied by the caller (a handshake rejection surfaces as [[DynexiteUnauthenticated]]). */
  private def upload(questionId: String, xml: String, expectedName: String, title: String,
                     reachablePoints: Points, cookie: String): Unit = {
    val url = URI.create(s"wss://dynexite.rwth-aachen.de/t/api/sub/builder/$questionId")
    val queue = new LinkedBlockingQueue[String]()

    val listener = new WebSocket.Listener {
      private val buffer = StringBuilder()
      override def onOpen(ws: WebSocket): Unit = ws.request(Long.MaxValue)
      override def onText(ws: WebSocket, data: CharSequence, last: Boolean): CompletionStage[?] = {
        buffer.append(data)
        if (last) { queue.put(buffer.toString); buffer.clear() }
        null
      }
      override def onError(ws: WebSocket, error: Throwable): Unit =
        logger.error(s"WebSocket error while uploading to Dynexite item $questionId", error)
    }

    def nextMessage(what: String, timeoutSeconds: Int): ujson.Value = {
      val raw = queue.poll(timeoutSeconds.toLong, TimeUnit.SECONDS)
      if (raw == null)
        throw RuntimeException(s"Timed out waiting for $what from Dynexite (item $questionId).")
      ujson.read(raw)
    }

    def receive(action: String, timeoutSeconds: Int): ujson.Value = {
      val deadline = System.nanoTime() + timeoutSeconds.toLong * 1000000000L
      while (System.nanoTime() < deadline) {
        val msg = nextMessage(s"'$action'", timeoutSeconds)
        if (msg.obj.get("action").map(_.str).contains(action)) return msg
      }
      throw RuntimeException(s"Timed out waiting for '$action' from Dynexite (item $questionId).")
    }

    /** Wait for the `OK` acknowledging the request with the given `msgId`, skipping unrelated
     * frames (`USER:JOIN`, `VALIDATION`, other clients' messages). Throws on an `ERROR` frame. */
    def receiveOk(msgId: String, timeoutSeconds: Int): ujson.Value = {
      val deadline = System.nanoTime() + timeoutSeconds.toLong * 1000000000L
      while (System.nanoTime() < deadline) {
        val msg = nextMessage(s"OK($msgId)", timeoutSeconds)
        msg.obj.get("action").map(_.str) match {
          case Some("ERROR") =>
            throw RuntimeException(s"Dynexite returned an error for item $questionId: " +
              msg.obj.get("data").map(ujson.write(_)).getOrElse("(no detail)"))
          case Some("OK") if msg.obj.get("msgId").map(_.str).contains(msgId) => return msg
          case _ => // skip
        }
      }
      throw RuntimeException(s"Timed out waiting for OK($msgId) from Dynexite (item $questionId).")
    }

    val client = HttpClient.newHttpClient()
    val ws =
      try client.newWebSocketBuilder()
        .header("Cookie", withCookieName(cookie))
        .header("Origin", "https://dynexite.rwth-aachen.de")
        .buildAsync(url, listener)
        .join()
      catch case e: CompletionException if e.getCause.isInstanceOf[WebSocketHandshakeException] =>
        val status = handshakeStatus(e.getCause).map(s => s"HTTP $s").getOrElse("no status")
        throw DynexiteUnauthenticated(s"handshake rejected ($status)", e.getCause)
    logger.info(s"Connected to Dynexite builder for item $questionId.")

    try {
      // The server opens with an INIT frame carrying the full item state (blocks, versions, ...).
      val init = receive("INIT", 30)
      val actualName = init("data")("item")("name").str
      if (actualName != expectedName)
        throw RuntimeException(s"Dynexite item $questionId is named '$actualName', but expected " +
          s"'$expectedName'. Refusing to upload; check dynexiteQuestionId / dynexiteQuestionName.")
      // `blocks` is null (not []) for a brand-new, empty item.
      val existingBlocks = init("data")("item")("blocks").arrOpt.map(_.toList).getOrElse(Nil)

      /** Create an (empty) STACK block on an item that has none, and return it. Two steps, as the
       * editor does: BLOCK:CREATE (server mints the block + uuid), then BLOCK:ADD (attach it). */
      def createStackBlock(): ujson.Value = {
        logger.info(s"Dynexite item $questionId ('$actualName') has no blocks; creating a STACK block.")
        ws.sendText(ujson.write(ujson.Obj(
          "action" -> "BLOCK:CREATE", "data" -> ujson.Obj("type" -> "stack"), "msgId" -> "1000")), true).get()
        val created = receiveOk("1000", 30)("data")("block")
        created("solutionPath") = ujson.Null
        ws.sendText(ujson.write(ujson.Obj(
          "action" -> "BLOCK:ADD", "data" -> ujson.Obj("block" -> created), "msgId" -> "1001")), true).get()
        receiveOk("1001", 30) // the empty block also triggers a (failing) VALIDATION, which we skip
        created
      }

      val (block, changeMsgId) = existingBlocks match {
        case Nil => (createStackBlock(), "1002")
        case single :: Nil =>
          val typ = single.obj.get("type").map(_.str).getOrElse("")
          if (typ != "stack")
            throw RuntimeException(s"Dynexite item $questionId ('$actualName') has a single block of " +
              s"type '$typ', but only STACK blocks are supported.")
          (single, "1001")
        case many =>
          throw RuntimeException(s"Dynexite item $questionId ('$actualName') has ${many.length} blocks; " +
            s"upload only supports items with exactly one (STACK) block.")
      }

      // The editor is a dumb echo: send the whole block back with questionXML + title replaced.
      block("logic").obj("questionXML") = ujson.Str(xml)
      block("logic").obj("language") = ujson.Str("en")
      block("task") = ujson.Str(title)
      // The block's reachable points live in `logic.points` as a decimal string (e.g. "99").
      block("logic").obj("points") = ujson.Str(
        reachablePoints.toBigDecimal.bigDecimal.stripTrailingZeros.toPlainString)
      def payload = ujson.Obj(
        "uuid" -> block("uuid"),
        "changeCategory" -> ujson.Str("configuration"),
        "changes" -> block,
        "solutionPath" -> ujson.Null,
        "task" -> ujson.Str(title))
      val frame = ujson.Obj(
        "action" -> ujson.Str("ELEMENT:CHANGED"),
        "data" -> ujson.Obj("editor" -> payload, "users" -> payload),
        "msgId" -> ujson.Str(changeMsgId))
      ws.sendText(ujson.write(frame), true).get()
      logger.info(s"Sent question XML to Dynexite item $questionId (msgId $changeMsgId).")

      // The server always answers our message with an OK ack (matching our msgId). When the
      // content actually changed it first re-parses server-side and sends a VALIDATION frame;
      // if that reports errors the upload was rejected. If nothing changed, no VALIDATION comes,
      // so we must not block on it — just wait for our OK.
      var acked = false
      while (!acked) {
        val msg = nextMessage("acknowledgement (OK)", 30)
        msg.obj.get("action").map(_.str) match {
          case Some("VALIDATION") if msg.obj.get("hasErrors").exists(_.bool) =>
            val errors = msg.obj.get("errorMap").map(formatValidationErrors).getOrElse("(no details)")
            throw RuntimeException(s"Dynexite rejected the question XML for item $questionId:\n$errors")
          case Some("OK") if msg.obj.get("msgId").map(_.str).contains(changeMsgId) =>
            acked = true
          case _ => // ignore unrelated frames (USER:JOIN, clean VALIDATION, broadcasts, ...)
        }
      }
      logger.info(s"Dynexite accepted the question for item $questionId (no validation errors).")
    } finally
      ws.sendClose(WebSocket.NORMAL_CLOSURE, "done")
  }

  private val logger = Logger[Dynexite.type]
}
