package assessments

import assessments.Comment.Format.markdown
import assessments.Comment.Kind
import assessments.Comment.Kind.{feedback, warning}
import assessments.Commenter.{Case, Reachable}
import org.apache.commons.text.StringEscapeUtils
import org.jsoup.Jsoup
import utils.Markdown

import scala.annotation.targetName
import scala.collection.mutable
import scala.util.boundary
import scala.util.boundary.{Label, break}

/** Object to collect grading comments and points. */
final class Commenter {
  var points: Points = 0 
  private val builder = Seq.newBuilder[Comment]
  @targetName("addString")
  def +=(comment: String): Unit = builder += Comment(text = comment, format = markdown, kind = feedback)
  @targetName("addComment")
  def +=(comment: Comment): Unit = builder += comment
  def comments: Seq[Comment] = builder.result()
  def clear(): Unit = builder.clear()
  /** Starts a block for grading a single subproblem.
   *
   * In the block, you can normally add comments using `+=`.
   * You can refer to the reachable points of the whole block as [[max]].
   * You can exit the block with a given number of points using [[done]]`(n)`.
   * (These will be automatically added to [[points]] and the block will be exited.
   * An additional comment will be added saying how many points out of how many
   * were added.)
   *
   * Example:
   * {{{
   * commenter.gradeBlock(40) {
   *   if (everything good)
   *     commenter += "Well done"
   *     done(max)
 *     if (so so)
   *     commenter += "So so"
   *     done(max/2)
   *   commenter += "Sorry"
   *   done(0)
   * }
   * }}}
   *
   * @param max Number of reachable points for this subproblem.
   * @param body A block which does grading for the subproblem
   * */
  def gradeBlock(max: Points)(body: Label[Points] ?=> Reachable ?=> Nothing): Unit = {
    given Reachable = new Reachable(max)
    val reached = boundary(body)
    assert(reached <= max)
    this += s"$reached out of $max points"
    points += reached
  }

  //noinspection ScalaUnreachableCode
  def combinatorialGrader(max: Points, distinctions: Seq[Seq[(String, String)]],
                          grades: Seq[(String,Points)])
                         (checker: Label[Boolean] ?=> Seq[Case] => Unit)
                         (implicit exceptionContext: ExceptionContext): Unit = {
    // Get the distinctions uses Case objects
    val distinctions2 = distinctions
      .map(options => options.map((name,description) => Case(name, description, options.map(_._1).toSet)))
    // get all combinations of cases
    val combos = distinctions2.foldLeft[Seq[Seq[Case]]](Seq(Seq.empty)) { (acc: Seq[Seq[Case]], additional: Seq[Case]) =>
      for (combo <- acc;
           add <- additional)
        yield combo.appended(add) }
    
    def getPoints(combo: Seq[Case]): Points = {
      val matches = grades.filter((pattern, points) =>
        val patternParts = pattern.split("-")
        assert(patternParts.length == combo.length)
        combo.zip(patternParts).forall((c,p) => p == "*" || c == p)
      )
      if (matches.isEmpty)
        throw ExceptionWithContext(s"No pattern matches ${combo.map(_.name).mkString("-")}")
      if (matches.length > 1)
        throw ExceptionWithContext(s"Several pattern matche ${combo.map(_.name).mkString("-")}, namely ${matches.map(_._1).mkString(", ")}")
      matches.head._2
    }
    
    // for each combo, get whether it fits, the grade, and a comment string
    val evaluated = for (combo <- combos)
      yield {
        val accepted = boundary[Boolean] {
          checker(combo)
          throw ExceptionWithContext(s"Checker in combinatorial grader returned without using break(true/false), for case ${combo.mkString("-")}")
        }
        val points = getPoints(combo)
        val comment = combo.map(_.description).filter(_.nonEmpty) match
          case Seq() => "Correct solution"
          case cases => "Correct solution, except: " + cases.mkString(", ")
        (combo, accepted, points, comment)
      }
//    println(evaluated)
    evaluated
      .filter((combo, accepted, points, comment) => accepted)
      .maxByOption((combo, accepted, points, comment) => (points, -comment.length)) // Most points, from those shortest comment
      match {
      case Some((_, _, points, comment)) =>
        this += s"$comment. $points out of $max points."
        this.points += points
      case None =>
        this += s"Incorrect. 0 out of $max points."
    }

//    println(s"Chosen: $combo $points $comment")

  }
}

object Commenter {
  /** To be used inside [[Commenter.gradeBlock]] */
  def max(using reachable: Reachable): Points = reachable.points
  /** To be used inside [[Commenter.gradeBlock]] */
  def done(points: Points)(using label: Label[Points]): Nothing = {
    break(points)
  }
  /** For internal use by [[Commenter.gradeBlock]] */
  final class Reachable private[Commenter] (private[Commenter] val points: Points) extends AnyVal

  final class Case(val name: String, val description: String, options: Set[String]) {
    override def equals(obj: Any): Boolean =
      throw new RuntimeException(".equals not supported on Case")
    def ==(name: String): Boolean =
      if (!options.contains(name))
        throw RuntimeException(s"""Unknown case "$name", must be one of: ${options.mkString(", ")}.""")
      name == this.name
    override def toString: String = s"Case($name)"
  }

  object Case {
    def unapply(arg: Case): Some[String] = Some(arg.name)
  }
}

case class Comment(val html: String, val kind: Comment.Kind) {
  def toPlaintext: String = {
    val text = try Jsoup.parse(html).text() catch case _ => html.trim
    kind match
      case Kind.feedback => text
      case Kind.warning => s"WARNING: $text"
      case Kind.debug => s"debug: $text"
  }
}

object Comment {
  enum Kind {
    case feedback, warning, debug
  }
  enum Format {
    case html, markdown, plain
  }
  def apply(text: String, kind: Kind, format: Format): Comment = {
    val html = format match
      case Format.html => text
      case Format.markdown => Markdown.markdownToHtml(text)
      case Format.plain => StringEscapeUtils.escapeHtml4(text)
    new Comment(html = html, kind = kind)
  }
  def warning(markdown: String): Comment = Comment(markdown, kind = Kind.warning, format = Format.markdown)
  def debug(markdown: String): Comment = Comment(markdown, kind = Kind.debug, format = Format.markdown)
}

