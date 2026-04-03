package assessments.pageelements

import assessments.{FileMapBuilder, Html, Plaintext}
import utils.IndentedInterpolator

case class MathElement(latex: String, inline: Boolean = true) extends StaticElement {
  override def renderHtml(context: RenderContext, associatedFiles: FileMapBuilder): Html =
    if (inline) Html.fromPlaintext(s"\\($latex\\)")
    else Html.fromPlaintext(s"\\[$latex\\]")

  def display: MathElement = copy(inline = false)
}

object MathElement {
  /** Types that can be safely inserted into a LaTeX formula */
  type Insertable = Int | MathElement | Double
  private def processInsertable(insertable: Insertable) = insertable match {
    case i: Int => i.toString
    case m: MathElement => s"{${m.latex}}"
    case d: Double => d.toString
  }
  extension (sc: StringContext) {
    def m(args: Insertable*): MathElement = {
      val latex = StringContext.standardInterpolator(process=identity, args.map(processInsertable), sc.parts)
      MathElement(latex)
    }
  }
  def m(latex: String) = MathElement(latex)
}