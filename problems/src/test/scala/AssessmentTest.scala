import Dynexite.Dynexite
import assessments.{Assessment, MultipleChoice, Points}
import org.scalatest.funsuite.AnyFunSuiteLike

import scala.language.experimental.genericNumberLiterals
import java.nio.file.{Files, Path, Paths}
import scala.jdk.StreamConverters.*

class AssessmentTest extends AnyFunSuiteLike {
}

object AssessmentTest {
    def main(args: Array[String]): Unit = {
        val assessmentsPath = Path.of("/home/unruh/r/assessments/data/exam-pqc/")
        //    val resultsPath = "/home/unruh/cloud/qis/lectures/pqc-2024/exam1/dynexite-download-detailed-results.json"
        val resultsPath = "/home/unruh/tmp/dynexite-download-detailed-results.json"

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
                    grade(assessment, item)
                }
            }
        }
    }

    def grade(assessment: Assessment, item: Dynexite.Item): Unit = {
        val blocks = item.blocks
        assert(blocks.nonEmpty)
        val blockType = blocks.head.`type`
        for (block <- blocks) assert(block.`type` == blockType)

        blockType match {
            case Dynexite.BlockType.classification => gradeClassification(assessment, item)
//            case Dynexite.BlockType.stack => gradeStack(assessment, item)
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
}
