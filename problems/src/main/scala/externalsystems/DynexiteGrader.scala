package externalsystems

import assessments.ExceptionContext.initialExceptionContext

import scala.language.experimental.genericNumberLiterals
import assessments.pageelements.AnswerElement
import assessments.{Assessment, Commenter, ElementName, ExceptionContext, ExceptionWithContext, Grader, GradingContext, Points}
import com.typesafe.scalalogging.Logger
import externalsystems.Dynexite.{ClassificationBlock, DynexiteResponses, StackBlock, getDynexiteAnswers}

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

    val DynexiteResponses(answers, dynexitePoints, dynexiteReachable) = getDynexiteAnswers(item, assessment)
    val graders = (for (case (_, grader: Grader) <- assessment.pageElements) yield grader).toSeq;
    assert(graders.size == 1, graders)
    val grader = graders.head

    val commenter = Commenter()
    grader.grade(GradingContext(
      answers = answers.map { (k, v) => (k, v) },
      registrationNumber = registrationNumber
    ), commenter)

    logger.debug("Comments: " + commenter.comments)

    val reachable = grader.reachablePoints

    logger.debug(s"Points: ${commenter.points} / $reachable")
    logger.debug(s"Dynexite: $dynexitePoints / $dynexiteReachable")

    val report = ListBuffer[String]()
    report += s"Problem: ${assessment.name}"
    report += s"Points: ${commenter.points} out of $reachable"
    report += "Comments:"
    for (comment <- commenter.comments)
      report += "* "+comment

    // Allowing some error in this check since Dynexite doesn't have rational points
//    assert((points - dynexitePoints).abs <= 0.005, (points, dynexitePoints))
    assert(reachable == dynexiteReachable)

    QuestionResult(points=commenter.points, reachable=reachable, report=report.mkString("\n"))
  }

  private val logger = Logger[DynexiteGrader.type]
}
