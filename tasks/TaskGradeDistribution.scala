import TaskContext.{gradeDistributionHtml, gradingResultSpreadsheet}
import assessments.GradingScale.{failGrade, passingGrades}
import assessments.Task
import externalsystems.Spreadsheet
import externalsystems.Spreadsheet.Format.CSV

import java.awt.Desktop
import java.io.PrintWriter
import java.nio.file.Path
import scala.collection.immutable.SeqMap
import scala.util.Using

/** Computes the grade distribution from [[gradingResultSpreadsheet]] (columns student;points;grade)
 * and writes it to [[gradeDistributionHtml]] as a single HTML file containing both a table and a
 * (CSS-only) histogram. Opens the file in a browser afterwards. */
object TaskGradeDistribution extends Task {
  /** All grades in display order (best first), including the fail grade. */
  private val allGrades: Seq[Double] = passingGrades appended failGrade

  private def gradeKey(grade: Double): String = f"$grade%.1f"

  private val examResults = Spreadsheet.load(gradingResultSpreadsheet, CSV.default)

  // Count occurrences per grade, keyed by the normalized "x.y" string.
  private val counts: Map[String, Int] =
    examResults.rows
      .map(row => gradeKey(row.grade.toDouble))
      .groupMapReduce(identity)(_ => 1)(_ + _)

  private val total = examResults.rows.length
  private val maxCount = allGrades.map(g => counts.getOrElse(gradeKey(g), 0)).maxOption.getOrElse(0)

  // Ordered distribution: every standard grade, including those with count 0.
  private val distribution: SeqMap[String, Int] =
    SeqMap.from(allGrades.map(g => gradeKey(g) -> counts.getOrElse(gradeKey(g), 0)))

  writeHtml()
  openInBrowser(gradeDistributionHtml)

  private def pct(count: Int): String =
    if (total == 0) "0.0" else f"${100.0 * count / total}%.1f"

  private def writeHtml(): Unit = {
    Using.resource(new PrintWriter(gradeDistributionHtml.toFile)) { writer =>
      writer.println(
        s"""<!DOCTYPE html>
           |<html>
           |<head>
           |<meta charset="utf-8">
           |<title>Grade distribution</title>
           |<style>
           |  body { font-family: sans-serif; margin: 2em; }
           |  table.dist { border-collapse: collapse; margin-bottom: 2em; }
           |  table.dist th, table.dist td { border: 1px solid #999; padding: 4px 10px; text-align: right; }
           |  table.dist th { background: #eee; }
           |  .histogram { display: grid; grid-template-columns: 3em 1fr; gap: 6px 10px; align-items: center; max-width: 700px; }
           |  .histogram .label { text-align: right; font-variant-numeric: tabular-nums; }
           |  .histogram .barcell { background: #f0f0f0; }
           |  .histogram .bar { background: #4a7dbf; color: white; padding: 2px 6px; white-space: nowrap; min-width: 1px; box-sizing: border-box; }
           |</style>
           |</head>
           |<body>
           |<h1>Grade distribution</h1>
           |<p>Total students: $total</p>
           |
           |<h2>Table</h2>
           |<table class="dist">
           |<tr><th>Grade</th><th>Count</th><th>Percent</th></tr>""".stripMargin)

      for ((grade, count) <- distribution)
        writer.println(s"<tr><td>$grade</td><td>$count</td><td>${pct(count)}%</td></tr>")

      writer.println(
        s"""<tr><th>Total</th><th>$total</th><th>100.0%</th></tr>
           |</table>
           |
           |<h2>Histogram</h2>
           |<div class="histogram">""".stripMargin)

      for ((grade, count) <- distribution) {
        val widthPct = if (maxCount == 0) 0.0 else 100.0 * count / maxCount
        writer.println(
          s"""  <div class="label">$grade</div>
             |  <div class="barcell"><div class="bar" style="width: $widthPct%">$count</div></div>""".stripMargin)
      }

      writer.println(
        """</div>
          |</body>
          |</html>""".stripMargin)
    }
    println(s"Grade distribution written to $gradeDistributionHtml")
  }

  private def openInBrowser(path: Path): Unit = {
    val uri = path.toUri
    try {
      if (Desktop.isDesktopSupported && Desktop.getDesktop.isSupported(Desktop.Action.BROWSE))
        Desktop.getDesktop.browse(uri)
      else
        new ProcessBuilder("xdg-open", uri.toString).inheritIO().start()
    } catch {
      case e: Throwable =>
        logger.warn(s"Could not open browser automatically: $e. Open manually: $uri")
    }
  }
}
