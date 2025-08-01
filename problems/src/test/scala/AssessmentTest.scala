import scala.language.experimental.genericNumberLiterals
import externalsystems.{Dynexite, DynexiteGrader, RWTHOnlineGrades}
import assessments.ExceptionContext.initialExceptionContext
import assessments.pageelements.AnswerElement
import assessments.{Assessment, ElementName, ExceptionContext, ExceptionWithContext, Grader, Points}
import com.github.tototoshi.csv.{CSVFormat, CSVReader, CSVWriter, DefaultCSVFormat, Quoting}
import externalsystems.Dynexite.{ClassificationBlock, StackBlock}
import externalsystems.DynexiteGrader.getDynexiteAnswers
import org.apache.commons.io.FileUtils
import org.log4s.getLogger
import org.scalatest.funsuite.AnyFunSuiteLike

import java.io.FileWriter
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.nio.file.Path.*
import scala.collection.mutable

class AssessmentTest extends AnyFunSuiteLike {
}

object AssessmentTest {


    def main(args: Array[String]): Unit = {
        /*
        val resultsPath = "/home/unruh/cloud/qis/lectures/pqc-2024/exam2/dynexite-exam-results.json"
        val examsPath = Path.of("/home/unruh/cloud/qis/lectures/pqc-2024/exam2/dynexite-archive-of-answers.zip")
        val targetDir = Path.of("/home/unruh/cloud/qis/lectures/pqc-2024/exam2/corrected1")

        val rwthData = RWTHOnlineGrades.load(Path.of("/home/unruh/cloud/qis/lectures/pqc-2024/exam2/rwthonline-result-table-for-filling.csv"))


        val results = Dynexite.parseExamResults(of(resultsPath))
        val exam = PqcExam2

        if (Files.exists(targetDir))
            FileUtils.deleteDirectory(targetDir.toFile)
        Files.createDirectory(targetDir)

        given CSVFormat = new DefaultCSVFormat {
            override val delimiter: Char = ';'
            override val lineTerminator: String = "\n"
        }
        val mails =  CSVWriter.open(targetDir.resolve("mails.csv").toFile)
        mails.writeRow(Seq("first","last","regno","email","pdf","txt"))

        for (learner <- results.learners
             if learner.attempts.nonEmpty) {
            println("\n\n\n")
            val result = DynexiteGrader.gradeLearner(learner, exam.questions.map(_.assessment))
            val pdf = DynexiteGrader.getAnswerPDF(examsPath, learner.identifier)
            Files.write(targetDir.resolve(s"${learner.identifier}.pdf"), pdf)
            Files.write(targetDir.resolve(s"${learner.identifier}.txt"), result.report.getBytes(StandardCharsets.UTF_8))
//            println(result)

//            val Seq(rwthEntry) = rwthData.filter(e => e("REGISTRATION_NUMBER") == learner.identifier)
            val rwthEntry = rwthData(learner.identifier)

            mails.writeRow(Seq(
                rwthEntry.firstName,
                rwthEntry.familyName,
                learner.identifier,
                rwthEntry.email,
                s"${learner.identifier}.pdf",
                s"${learner.identifier}.txt",
                ))

            rwthEntry.grade = result.grade
            rwthEntry.remark = result.shortReport
        }

        mails.close()
        rwthData.save(Path.of("/home/unruh/cloud/qis/lectures/pqc-2024/exam2/rwthonline-result-table-for-upload.csv"))
        
   
         */
    }

    private val logger = getLogger
}


