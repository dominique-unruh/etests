import scala.language.experimental.genericNumberLiterals
import externalsystems.Dynexite
import assessments.ExceptionContext.initialExceptionContext
import assessments.pageelements.{AnswerElement, MultipleChoice}
import assessments.stack.StackMath.{Bool, Funcall, Integer, Operation, Ops, Variable}
import assessments.{Assessment, Context, ElementName, ExceptionContext, ExceptionWithContext, Grader, Points, SyntaxError, TextInput, UserError}
import exam.PqcExam2
import externalsystems.Dynexite.{ClassificationBlock, StackBlock}
import fastparse.Parsed
import me.shadaj.scalapy
import me.shadaj.scalapy.py
import org.scalatest.funsuite.AnyFunSuiteLike
import utils.Python

import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable
import scala.jdk.StreamConverters.*
import scala.math.abs

class AssessmentTest extends AnyFunSuiteLike {
}

object AssessmentTest {
    def main(args: Array[String]): Unit = {
        val resultsPath = "/home/unruh/cloud/qis/lectures/pqc-2024/exam2/dynexite-exam-results.json"

        val results = Dynexite.parseExamResults(Path.of(resultsPath))
        val assessments = PqcExam2.questions

        for (learner <- results.learners) {
            given ExceptionContext = initialExceptionContext(s"Grading for learner ${learner.learnerId}", learner)
            assert(learner.attempts.length <= 1)
            for (attempt <- learner.attempts) {
//                assert(attempt.items.length == assessments.length, (attempt.items.length, assessments.length))
                // TODO: Reinstantiate length check
                for ((item, assessment) <- attempt.items.zip(assessments)) {
                    if (assessment == null) {
                        // TODO raise error
                        println("WARNING: Missing assessment")
                    } else {
                        given ExceptionContext = initialExceptionContext(s"Grading assessment ${assessment.name}", assessment)
                        grade(learner.identifier, assessment, item)
                    }
                }
            }
        }
    }

    def grade(studentRegistration: String, assessment: Assessment, item: Dynexite.Item)(using exceptionContext: ExceptionContext): Unit = {
        given ExceptionContext = exceptionContext.add(s"Correcing assessment ${assessment.name}", assessment, item)
        val blocks = item.blocks
        assert(blocks.nonEmpty)
//        val blockType = blocks.head.`type`
//        for (block <- blocks) assert(block.`type` == blockType)

        grade2(studentRegistration, assessment, item)
    }

    // TODO remove
/*
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
                val isCorrect = answerTag == element.reference
                if (isCorrect) points += element.points
            }
        }
        assert(item.earnedPoints == points, (item.earnedPoints, points))
        assert(item.maxPoints == maxPoints, (item.maxPoints, maxPoints))
    }
*/

    def getDynexiteAnswersStack(item: Dynexite.Item, assessment: Assessment)
                               (implicit exceptionContext: ExceptionContext):
    (Map[ElementName, String], Points, Points) = {
        var points: Points = 0
        var reachable: Points = 0

        val expectedNames = {
            val builder = mutable.Map[String, ElementName]()
            for (case (name, _: AnswerElement[String]) <- assessment.pageElements) {
                val lastName = name.last
                assert(!builder.contains(lastName), (builder, lastName, assessment.pageElements))
                builder.update(lastName, name)
            }
            builder.toMap
        }

        val answers = mutable.Map[ElementName,String]()

        for (block <- item.blocks) {
            reachable += block.maxPoints
            points += block.earnedPoints;

            for ((name, value) <- block.asInstanceOf[StackBlock].answers) {
                val elementName = expectedNames(name)
                assert(!answers.contains(elementName))
                answers.update(elementName, value)
            }
        };

        (answers.toMap, points, reachable)
    }

    def getDynexiteAnswersClassification(item: Dynexite.Item, assessment: Assessment)
                               (implicit exceptionContext: ExceptionContext):
    (Map[ElementName, String], Points, Points) = {
        var points: Points = 0
        var reachable: Points = 0

        val dynexiteAnswers =
            for (block <- item.blocks;
                answer <- block.asInstanceOf[ClassificationBlock].answers)
                yield answer

        val assessmentNames = (for (case (name, _: AnswerElement[?]) <- assessment.pageElements)
            yield name).toSeq;

        assert(dynexiteAnswers.length == assessmentNames.length, (dynexiteAnswers, assessmentNames))

        val answers = mutable.Map[ElementName,String]()

        for (block <- item.blocks) {
            reachable += block.maxPoints
            points += block.earnedPoints;
        };
        
        for ((name,answer) <- assessmentNames.zip(dynexiteAnswers)) {
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


    // TODO inline
    def grade2(studentRegistration: String, assessment: Assessment, item: Dynexite.Item)(implicit exceptionContext: ExceptionContext): Unit = {
        val (answers, dynexitePoints, dynexiteReachable) = getDynexiteAnswers(item, assessment)
        val graders = (for (case (_, grader: Grader) <- assessment.pageElements) yield grader).toSeq;
        assert(graders.size == 1, graders)
        val grader = graders.head

        val (points, comments) = grader.grade(answers.map { (k, v) => (k, v) })
        val reachable = grader.points;

        println(s"\nREPORT (assessment ${assessment.name}, student $studentRegistration)")
        println(s"Points: $points / $reachable")
        println(s"Dynexite: $dynexitePoints / $dynexiteReachable")
        println("COMMENTS:")
        for (comment <- comments)
            println("* "+comment)
        println("\n\n")

        // Allowing some error in this check since Dynexite doesn't do exact arithmetic
        assert((points - dynexitePoints).abs <= 0.005, (points, dynexitePoints))
        assert(reachable == dynexiteReachable)
    }
}


