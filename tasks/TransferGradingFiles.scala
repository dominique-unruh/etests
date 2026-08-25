import TaskContext.{gradingReportDir, scieboReportDir}
import assessments.Task

import java.nio.file.Files
import scala.sys.process.*

/** Copy the result of grading ([[gradingReportDir]]) to Sciebo folder ([[scieboReportDir]]) */
object TransferGradingFiles extends Task {
  Files.createDirectories(scieboReportDir)

  // Trailing "/" on the source so rsync copies the *contents* of gradingReportDir into scieboReportDir.
  val command = Seq(
    "rsync", "-av", "--delete",
    s"$gradingReportDir/",
    scieboReportDir.toString)

  logger.info(s"Running: ${command.mkString(" ")}")
  val exitCode = command.!
  if (exitCode != 0)
    sys.error(s"rsync failed with exit code $exitCode")
}
