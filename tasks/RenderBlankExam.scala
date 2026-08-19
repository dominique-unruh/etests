import assessments.{Exam, Task}

/** Temporary task: render a blank (no-answers) printout PDF of the current exam.
 *
 * Renders every problem statically with no student answers (so solution-only content — explanations,
 * grading rules — is omitted), concatenates them, and writes a single PDF via `Exam.renderExam`
 * (Docker's `html-to-pdf` converter). Output goes next to the exam sources as `exam-printout.pdf`.
 *
 * Run with `sbt "project tasks; runMain RenderBlankExam"`; the exam is the `current.exam` option
 * (full `Exam` class name; prompted if unset). See `doc/exporting.md` (Printouts).
 */
object RenderBlankExam extends Task {
  val examName = Task.Option[String]("exam to render (full class name)", "current.exam")
  val exam = getClass.getClassLoader.loadClass(examName.value + "$").getField("MODULE$")
    .get(null).asInstanceOf[Exam]

  val outputFile = exam.sourceFile.getParent.resolve("exam-printout.pdf")
  exam.renderExam(outputFile, showSolutions = false)
  println(s"Wrote blank exam printout to $outputFile")
}
