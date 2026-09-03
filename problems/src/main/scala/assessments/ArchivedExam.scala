package assessments

import assessments.InterpolatedMarkdown.md
import assessments.pageelements.{Element, RenderContext, StaticElement}
import io.circe.Json
import io.github.classgraph.ClassGraph
import org.apache.commons.text.StringEscapeUtils.escapeHtml4

import java.nio.charset.StandardCharsets.UTF_8
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
        .map((json, index) => new Problem(subdir, json, index))
    catch case _: Throwable => Seq.empty

  /** Parsed root of the single `*.yaml` manifest in `<subdir>/archive/` on the classpath. */
  private def archiveYaml(subdir: String): Option[Json] = {
    val scan = new ClassGraph().acceptPaths(s"$subdir/archive").scan()
    try
      scan.getResourcesWithExtension("yaml").asScala.headOption
        .flatMap(resource => io.circe.yaml.parser.parse(resource.getContentAsString).toOption)
    finally scan.close()
  }

  /** Bytes of a file in `<subdir>/archive/` on the classpath, or None if absent. */
  private def readArchiveFile(subdir: String, filename: String): Option[Array[Byte]] =
    Option(getClass.getResourceAsStream(s"/$subdir/archive/$filename")).map(_.readAllBytes())

  /** A single archived problem, reconstructed from one dict of the manifest's `problems:` array.
   * Has no interactive content or grading. Construction never fails: any missing/invalid field
   * (including `json` not being a dict at all) falls back to a default. */
  class Problem(subdir: String, json: Json, index: Int) extends MarkdownAssessment {
    override val name: String =
      json.hcursor.get[String]("name").toOption.getOrElse(s"Problem ${index + 1}")
    override val id: String = s"problem$index"
    override lazy val reachablePoints: Points =
      json.hcursor.get[BigDecimal]("reachablePoints").toOption.map(Points(_)).getOrElse(Points.zero)

    override lazy val question: InterpolatedMarkdown[Element | HtmlConvertible] = {
      val rendered = json.hcursor.get[Vector[String]]("rendered").toOption.getOrElse(Vector.empty)
      val pdf: Option[Array[Byte]] =
        rendered.find(_.toLowerCase.endsWith(".pdf")).flatMap(readArchiveFile(subdir, _))
      val html: Option[Html] =
        rendered.find(_.toLowerCase.endsWith(".html"))
          .flatMap(readArchiveFile(subdir, _)).map(bytes => Html(new String(bytes, UTF_8)))

      md"""
        ${YamlElement(io.circe.yaml.Printer.spaces2.pretty(json))}

        ${maybeElement(pdf.map(PDFLinkElement(_)))}

        ${html.getOrElse(Html(""))}

        ${if (pdf.isEmpty && html.isEmpty) Html("Question content not available in Archive") else Html("")}
      """
    }

    private def maybeElement(element: Option[StaticElement | HtmlConvertible]) : StaticElement | HtmlConvertible = element match {
      case Some(e) => e
      case None => Html("")
    }
  }

  /** A problem's manifest dict, rendered as a code block. */
  class YamlElement(yaml: String) extends StaticElement {
    override def renderHtml(context: RenderContext, associatedFiles: FileMapBuilder): Html =
      Html(s"<pre><code>${escapeHtml4(yaml)}</code></pre>")
  }

  /** A link to a rendered problem PDF, embedded as an associated file. */
  class PDFLinkElement(pdf: Array[Byte]) extends StaticElement {
    override def renderHtml(context: RenderContext, associatedFiles: FileMapBuilder): Html = {
      val url = associatedFiles.add(basename = "problem", extension = "pdf",
        mimeType = "application/pdf", content = pdf)
      Html(s"""<a href="${escapeHtml4(url)}">Question as PDF</a>""")
    }
  }
}
