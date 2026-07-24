package assessments

import io.github.classgraph.ClassGraph
import utils.Tag.Tags

import scala.jdk.CollectionConverters.*

/** A placeholder exam for an archived subdirectory of `exams/` (one listed under `archived:`
 * in `exams/exams.yaml`). It has a name but no problems; its content lives only as the static
 * snapshot in `<subdir>/archive/`. Discovered and instantiated in [[Exam.exams]]. */
class ArchivedExam(subdir: String) extends Exam(ArchivedExam.archivedName(subdir))() {
  override val id: String = s"$subdir.Exam"
}

object ArchivedExam {
  /** Name for the archived exam: the `name` from its `<subdir>/archive/` yaml manifest, or the
   * subdir itself if that cannot be determined, in both cases suffixed with " (archived)". */
  private def archivedName(subdir: String): String = {
    val base =
      try nameFromArchive(subdir).getOrElse(subdir)
      catch case _: Throwable => subdir
    s"$base (archived)"
  }

  private def nameFromArchive(subdir: String): Option[String] = {
    val scan = new ClassGraph().acceptPaths(s"$subdir/archive").scan()
    try
      scan.getResourcesWithExtension("yaml").asScala.headOption.flatMap { resource =>
        io.circe.yaml.parser.parse(resource.getContentAsString).toOption
          .flatMap(_.hcursor.get[String]("name").toOption)
      }
    finally scan.close()
  }
}
