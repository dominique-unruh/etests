package assessments

import assessments.InterpolatedMarkdown.md
import assessments.pageelements.Element
import io.circe.Json
import io.github.classgraph.ClassGraph

import scala.jdk.CollectionConverters.*

/** A placeholder exam for an archived subdirectory of `exams/` (one listed under `archived:`
 * in `exams/exams.yaml`). Its name and (content-less) problems are reconstructed from the static
 * snapshot in `<subdir>/archive/`. Discovered and instantiated in [[Exam.exams]]. */
class ArchivedExam(subdir: String)
  extends Exam(ArchivedExam.archivedName(subdir))(ArchivedExam.archivedProblems(subdir)*) {
  override val id: String = s"$subdir.Exam"
}

object ArchivedExam {
  /** Name for the archived exam: the `name` from its `<subdir>/archive/` yaml manifest, or the
   * subdir itself if that cannot be determined, in both cases suffixed with " (archived)". */
  private def archivedName(subdir: String): String = {
    val base =
      try archiveYaml(subdir).flatMap(_.hcursor.get[String]("name").toOption).getOrElse(subdir)
      catch case _: Throwable => subdir
    s"$base (archived)"
  }

  /** One [[Problem]] per entry in the `problems:` array of the `<subdir>/archive/` yaml manifest.
   * Empty if there is no such array (or it cannot be read). */
  def archivedProblems(subdir: String): Seq[Problem] =
    try
      archiveYaml(subdir)
        .flatMap(_.hcursor.get[Vector[Json]]("problems").toOption)
        .getOrElse(Vector.empty)
        .zipWithIndex
        .map((json, index) => new Problem(json, index))
    catch case _: Throwable => Seq.empty

  /** Parsed root of the single `*.yaml` manifest in `<subdir>/archive/` on the classpath. */
  private def archiveYaml(subdir: String): Option[Json] = {
    val scan = new ClassGraph().acceptPaths(s"$subdir/archive").scan()
    try
      scan.getResourcesWithExtension("yaml").asScala.headOption
        .flatMap(resource => io.circe.yaml.parser.parse(resource.getContentAsString).toOption)
    finally scan.close()
  }

  /** A single archived problem, reconstructed from one dict of the manifest's `problems:` array.
   * Has no interactive content or grading. Construction never fails: any missing/invalid field
   * (including `json` not being a dict at all) falls back to a default. */
  class Problem(json: Json, index: Int) extends MarkdownAssessment {
    override val name: String =
      json.hcursor.get[String]("name").toOption.getOrElse(s"Problem ${index + 1}")
    override val id: String = s"problem$index"
    override lazy val reachablePoints: Points =
      json.hcursor.get[BigDecimal]("reachablePoints").toOption.map(Points(_)).getOrElse(Points.zero)
    override lazy val question: InterpolatedMarkdown[Element | HtmlConvertible] =
      md"*(Archived problem — content available only as a static snapshot.)*"
    override def grade()(using context: GradingContext, exceptionContext: ExceptionContext): Unit =
      throw NoGraderYetException
  }
}
