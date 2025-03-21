import scala.language.experimental.genericNumberLiterals
import externalsystems.Dynexite
import assessments.ExceptionContext.initialExceptionContext
import assessments.pageelements.{AnswerElement, MultipleChoice}
import assessments.stack.StackMath.{Bool, Funcall, Integer, Operation, Ops, Variable}
import assessments.{Assessment, Context, ElementName, ExceptionContext, ExceptionWithContext, Grader, Points, SyntaxError, TextInput, UserError}
import exam.PQC_Exam_2
import fastparse.Parsed
import me.shadaj.scalapy
import me.shadaj.scalapy.py
import org.scalatest.funsuite.AnyFunSuiteLike
import utils.Python

import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable
import scala.jdk.StreamConverters.*

class AssessmentTest extends AnyFunSuiteLike {
}

object AssessmentTest {
    def main(args: Array[String]): Unit = {
        val resultsPath = "/home/unruh/cloud/qis/lectures/pqc-2024/exam2/dynexite-exam-results.json"

        val results = Dynexite.parseExamResults(Path.of(resultsPath))
        val assessments = for (assessment <- PQC_Exam_2) yield
            assessment.assessment

        for (learner <- results.learners) {
            assert(learner.attempts.length <= 1)
            for (attempt <- learner.attempts) {
                for ((item, assessment) <- attempt.items.zip(assessments)) {
                    given ExceptionContext = initialExceptionContext(s"Grading for learner ${learner.learnerId}", learner)
                    grade(learner.identifier, assessment, item)
                }
            }
        }
    }

    def grade(studentRegistration: String, assessment: Assessment, item: Dynexite.Item)(using exceptionContext: ExceptionContext): Unit = {
        given ExceptionContext = exceptionContext.add(s"Correcing assessment ${assessment.name}", assessment, item)
        val blocks = item.blocks
        assert(blocks.nonEmpty)
        val blockType = blocks.head.`type`
        for (block <- blocks) assert(block.`type` == blockType)

        blockType match {
            case Dynexite.BlockType.classification => gradeClassification(assessment, item)
            case Dynexite.BlockType.stack => gradeStack(studentRegistration, assessment, item)
        }
    }

    // TODO remove
    def gradeClassification(assessment: Assessment, item: Dynexite.Item): Unit = {
        var points: Points = 0
        var maxPoints: Points = 0
        val answers = for (block0 <- item.blocks;
                           block = block0.asInstanceOf[Dynexite.ClassificationBlock];
                           answer <- block.answers)
            yield answer

        assert(answers.length == assessment.pageElements.size)

        for (((name,element_), answer) <- assessment.pageElements.zip(answers)) {
            val element = element_.asInstanceOf[MultipleChoice]
            maxPoints += element.points
            if (answer != null) {
                val Seq((answerTag, _)) = element.options.to(Seq).filter((k, v) => v == answer)
                val isCorrect = answerTag == element.correct
                if (isCorrect) points += element.points
            }
        }
        assert(item.earnedPoints == points, (item.earnedPoints, points))
        assert(item.maxPoints == maxPoints, (item.maxPoints, maxPoints))
    }

    def getDynexiteAnswers(studentRegistration: String, item: Dynexite.Item, assessment: Assessment)
                          (implicit exceptionContext: ExceptionContext):
                    (Map[ElementName, String], Points, Points) = {
        var points: Points = 0
        var reachable: Points = 0

        val expectedAnswerNames: mutable.ArrayDeque[ElementName] =
            (for (case (name, pageElement: AnswerElement[?]) <- assessment.pageElements)
                yield name)
              .to(mutable.ArrayDeque)

        val answers = mutable.Map[ElementName,String]()

        for (block <- item.blocks) {
            reachable += block.maxPoints
            points += block.earnedPoints

            block match
                case block: Dynexite.ClassificationBlock =>
                    ???
                case block: Dynexite.StackBlock =>
                    val expected = {
                        val builder = mutable.Map[String, ElementName]()
                        for (_ <- block.answers) {
                            val name = expectedAnswerNames.removeHead()
                            val lastName = name.last
                            assert(!builder.contains(lastName))
                            builder.update(lastName, name)
                        }
                        builder.toMap
                    }
                    assert(block.answers.keySet == expected.keySet)
                    for ((name, value) <- block.answers) {
                        val elementName = expected(name)
                        assert(!answers.contains(elementName))
                        answers.update(elementName, value)
                    }
        }

        (answers.toMap, points, reachable)
    }


    def gradeStack(studentRegistration: String, assessment: Assessment, item: Dynexite.Item)(implicit exceptionContext: ExceptionContext): Unit = {
        val (answers, dynexitePoints, dynexiteReachable) = getDynexiteAnswers(studentRegistration, item, assessment)
        val graders = (for (case (_, grader: Grader) <- assessment.pageElements) yield grader).toSeq;
        assert(graders.size == 1, graders)
        val grader = graders.head

        val (points, comments) = grader.grade(answers.map { (k, v) => (k, v) })
        val reachable = grader.points;

        println(s"\nREPORT (assessment ${assessment.name}, student $studentRegistration)")
        println(s"Points: $points / $reachable")
        println(s"Dynexite: $dynexitePoints / $dynexiteReachable")
        assert(points == dynexitePoints)
        assert(reachable == dynexiteReachable)
        println("COMMENTS:")
        for (comment <- comments)
            println("* "+comment)
        println("\n\n")
    }
}


