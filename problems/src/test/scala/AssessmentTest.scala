import scala.language.experimental.genericNumberLiterals
import Dynexite.Dynexite
import assessments.ExceptionContext.initialExceptionContext
import assessments.stack.StackMath.{Bool, Funcall, Integer, Operation, Ops, Variable}
import assessments.{Assessment, Context, ElementName, ExceptionContext, ExceptionWithContext, Grader, MultipleChoice, Points, Python, SyntaxError, TextInput, UserError}
import fastparse.Parsed
import me.shadaj.scalapy
import me.shadaj.scalapy.py
import org.scalatest.funsuite.AnyFunSuiteLike

import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable
import scala.jdk.StreamConverters.*

class AssessmentTest extends AnyFunSuiteLike {
}

object AssessmentTest {
    def main(args: Array[String]): Unit = {
        val assessmentsPath = Path.of("/home/unruh/r/assessments/data/exam-2-pqc/")
        //    val resultsPath = "/home/unruh/cloud/qis/lectures/pqc-2024/exam1/dynexite-download-detailed-results.json"
        val resultsPath = "/home/unruh/cloud/qis/lectures/pqc-2024/exam2/dynexite-exam-results.json"

        val results = Dynexite.parseExamResults(Path.of(resultsPath))
        val assessmentPaths = Files.list(assessmentsPath).toScala(Seq).sorted()
        val assessments = for (path <- assessmentPaths) yield
            Assessment.fromMarkdownFile(path)

        for (learner <- results.learners) {
//            println(learner.identifier)
            assert(learner.attempts.length <= 1)
            for (attempt <- learner.attempts) {
//                assert(attempt.items.length == assessments.length) // TODO
                for ((item, assessment) <- attempt.items.zip(assessments)) {
                    given ExceptionContext = initialExceptionContext(s"Grading for learner ${learner.learnerId}", learner)
                    grade(assessment, item)
                }
            }
        }
    }

    def grade(assessment: Assessment, item: Dynexite.Item)(using exceptionContext: ExceptionContext): Unit = {
        given ExceptionContext = exceptionContext.add(s"Correcing assessment ${assessment.name}", assessment, item)
        val blocks = item.blocks
        assert(blocks.nonEmpty)
        val blockType = blocks.head.`type`
        for (block <- blocks) assert(block.`type` == blockType)

        blockType match {
            case Dynexite.BlockType.classification => gradeClassification(assessment, item)
            case Dynexite.BlockType.stack => gradeStack(assessment, item)
        }
    }

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
//                println(s"$answer -> $answerTag / ${element.correct}")
//                println(s"EP ${element.points} $isCorrect")
//                println(s"P ${points}")
            }
        }
        // 1.45 + 1.45 + 1.4 = 4.3
//        println("-------")
//        println(item.blocks.map(_.answers))
//        println(answers)
        assert(item.earnedPoints == points, (item.earnedPoints, points))
        assert(item.maxPoints == maxPoints, (item.maxPoints, maxPoints))
    }

    def gradeStack(assessment: Assessment, item: Dynexite.Item)(implicit exceptionContext: ExceptionContext): Unit = {
        var points: Points = 0
        var maxPoints: Points = 0
        val answers = {
            val builder = mutable.HashMap[String,String]()
            for (block0 <- item.blocks;
                 block = block0.asInstanceOf[Dynexite.StackBlock];
                 (k,v) <- block.answers) {
                assert(!builder.contains(k))
                builder.put(k,v)
            }
            builder.to(Map)
        }

        if (!answers.keySet.subsetOf(assessment.pageElements.keys.toSet.map(_.toString)))
            throw ExceptionWithContext(s"Got answers in Dynexite for non-existing problems: ${answers.keySet.removedAll(assessment.pageElements.keys.map(_.toString)).mkString(", ")}",
                answers, assessment.pageElements.keys)
//        assert(answers.keySet.subsetOf(assessment.pageElements.keys.toSet.map(_.toString)), (answers, assessment.pageElements.keys))

        val graders = assessment.pageElements.collect {
            case (_, grader: Grader) => grader
        }

        if (graders.nonEmpty) {
            println("With grader")
            assert(graders.size == 1, graders)
            val grader = graders.head
            points = grader.grade(answers.map { (k, v) => (ElementName(k), v) })
            maxPoints = grader.points
            println((points, maxPoints))
        } else {
            println("Without grader")
            for ((name, element_) <- assessment.pageElements) {
                val element = element_.asInstanceOf[TextInput]
                maxPoints += element.points
                answers.get(element.name.toString) match {
                    case Some(answer) =>
                        if (element.correct.contains(answer))
                            points += element.points
                        else if (element.wrong.contains(answer)) {}
                        else if (element.partiallyCorrect.contains(answer))
                            points += element.partiallyCorrect(answer)
                        else
                            throw RuntimeException(s"Unexpected answer \"$answer\" to \"${element.name}\"")
                    case None =>
                }
            }
        }
        // 1.45 + 1.45 + 1.4 = 4.3
        //        println("-------")
        //        println(answers)
        assert(item.earnedPoints == points, (item.earnedPoints, points))
        assert(item.maxPoints == maxPoints, (item.maxPoints, maxPoints))
    }
}


