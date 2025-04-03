package externalsystems

import assessments.ExceptionContext.initialExceptionContext

import scala.language.experimental.genericNumberLiterals
import assessments.pageelements.AnswerElement
import assessments.{Assessment, ElementName, ExceptionContext, ExceptionWithContext, Grader, GradingContext, Points}
import externalsystems.Dynexite.{ClassificationBlock, StackBlock}
import org.log4s.getLogger

import java.io.FileInputStream
import java.nio.file.{Files, Path, Paths}
import java.util.zip.{ZipEntry, ZipFile, ZipInputStream}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.util.control.Breaks.{break, breakable}

object DynexiteGrader {
  case class Result(points: Points, reachable: Points, report: String, shortReport: String, grade: String)
  case class QuestionResult(points: Points, reachable: Points, report: String)

  def getAnswerPDF(archive: Path, registrationNumber: String): Array[Byte] = {
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

  def pointsToGrade(points: Points, reachable: Points): String = {
    logger.debug(s"Reachable: $reachable")
    assert(reachable == Points(100))
    if (points >= 95) "1.0"
    else if (points >= 90) "1.3"
    else if (points >= 85) "1.7"
    else if (points >= 80) "2.0"
    else if (points >= 75) "2.3"
    else if (points >= 70) "2.7"
    else if (points >= 65) "3.0"
    else if (points >= 60) "3.3"
    else if (points >= 55) "3.7"
    else if (points >= 50) "4.0"
    else "5.0"
  }

  def gradeLearner(learner: Dynexite.Learner, examQuestions: Seq[Assessment]): Result = {
    given ExceptionContext = initialExceptionContext(s"Grading for learner ${learner.identifier}", learner)

    logger.debug(s"Grading learner ${learner.learnerId}, ${learner.identifier}")
    assert(learner.attempts.length == 1)

    val reports = ListBuffer[String]()
    var totalPoints = Points(0)
    var totalReachable = Points(0)

    for (attempt <- learner.attempts) {
      assert(attempt.items.length == examQuestions.length, (attempt.items.length, examQuestions.length))
      for ((item, assessment) <- attempt.items.zip(examQuestions)) {
        given ExceptionContext = initialExceptionContext(s"Grading assessment ${assessment.name}", assessment)
        val result = gradeQuestion(registrationNumber = learner.identifier, assessment = assessment, item = item)
        logger.debug(s"Result: $result")
        reports += result.report
        totalPoints += result.points
        totalReachable += result.reachable
      }
    }

    logger.debug(s"$totalPoints, $totalReachable")
    val grade = pointsToGrade(totalPoints, totalReachable)
    val header = s"Student: ${learner.identifier}\n" +
      s"Total points: $totalPoints out of $totalReachable\n" +
      s"Grade: $grade\n\n" +
      s"The problems here are not in the same order as in the PDF, sorry."
    reports.insert(0, header)

    val shortReport = s"$totalPoints points out of $totalReachable"
    
    Result(points=totalPoints, reachable=totalReachable, grade=grade, 
      report=reports.mkString("\n\n"), shortReport = shortReport)
  }

  def gradeQuestion(registrationNumber: String, assessment: Assessment, item: Dynexite.Item)(using exceptionContext: ExceptionContext): QuestionResult = {
    logger.debug(s"Grading problem: ${assessment.name}")

    given ExceptionContext = exceptionContext.add(s"Correcting assessment ${assessment.name}", assessment, item)
    val blocks = item.blocks
    assert(blocks.nonEmpty)

    val (answers, dynexitePoints, dynexiteReachable) = getDynexiteAnswers(item, assessment)
    val graders = (for (case (_, grader: Grader) <- assessment.pageElements) yield grader).toSeq;
    assert(graders.size == 1, graders)
    val grader = graders.head

    val (points, comments) = grader.grade(GradingContext(
      answers = answers.map { (k, v) => (k, v) },
      registrationNumber = registrationNumber
    ))

    logger.debug("Comments: " + comments)

    val reachable = grader.points

    logger.debug(s"Points: $points / $reachable")
    logger.debug(s"Dynexite: $dynexitePoints / $dynexiteReachable")

    val report = ListBuffer[String]()
    report += s"Problem: ${assessment.name}"
    report += s"Points: $points out of $reachable"
    report += "Comments:"
    for (comment <- comments)
      report += "* "+comment

    // Allowing some error in this check since Dynexite doesn't have rational points
//    assert((points - dynexitePoints).abs <= 0.005, (points, dynexitePoints))
    assert(reachable == dynexiteReachable)

    QuestionResult(points=points, reachable=reachable, report=report.mkString("\n"))
  }

  private def getDynexiteAnswersStack(item: Dynexite.Item, assessment: Assessment)
                             (implicit exceptionContext: ExceptionContext):
  (Map[ElementName, String], Points, Points) = {
    var points: Points = 0
    var reachable: Points = 0

    val expectedNames = {
      val builder = mutable.Map[String, ElementName]()
      for (case (name, _: AnswerElement) <- assessment.pageElements) {
        val lastName = name.last
        assert(!builder.contains(lastName), (builder, lastName, assessment.pageElements))
        builder.update(lastName, name)
      }
      builder.toMap
    }

    val answers = mutable.Map[ElementName, String]()

    for (block <- item.blocks) {
      reachable += block.maxPoints
      points += block.earnedPoints

      for ((name, value) <- block.asInstanceOf[StackBlock].answers) {
        val elementName = expectedNames(name)
        assert(!answers.contains(elementName))
        answers.update(elementName, value)
      }
    }

    (answers.toMap, points, reachable)
  }

  private def getDynexiteAnswersClassification(item: Dynexite.Item, assessment: Assessment)
                                      (implicit exceptionContext: ExceptionContext):
  (Map[ElementName, String], Points, Points) = {
    var points: Points = 0
    var reachable: Points = 0

    val dynexiteAnswers =
      for (block <- item.blocks;
           answer <- block.asInstanceOf[ClassificationBlock].answers)
      yield answer

    val assessmentNames = (for (case (name, _: AnswerElement) <- assessment.pageElements)
      yield name).toSeq;

    assert(dynexiteAnswers.length == assessmentNames.length, (dynexiteAnswers, assessmentNames))

    val answers = mutable.Map[ElementName, String]()

    for (block <- item.blocks) {
      reachable += block.maxPoints
      points += block.earnedPoints;
    };

    for ((name, answer) <- assessmentNames.zip(dynexiteAnswers)) {
      assert(!answers.contains(name), (answers, name, assessmentNames))
      answers.update(name, answer)
    };

    (answers.toMap, points, reachable)
  }

  def getDynexiteAnswers(item: Dynexite.Item, assessment: Assessment)
                        (implicit exceptionContext: ExceptionContext):
  (Map[ElementName, String], Points, Points) = {
    item.blocks match {
      case Seq() => (Map.empty, 0, 0)
      case Seq(_: StackBlock, _*) =>
        assert(item.blocks.forall(_.isInstanceOf[StackBlock]))
        getDynexiteAnswersStack(item, assessment)
      case Seq(_: ClassificationBlock, _*) =>
        assert(item.blocks.forall(_.isInstanceOf[ClassificationBlock]))
        getDynexiteAnswersClassification(item, assessment)
      case Seq(block, _*) =>
        throw ExceptionWithContext(s"Dynexite data contained a solution block of unsupported type ${block.getClass.getName}")
    }
  }

  private val logger = getLogger
}
