package assessments.pageelements

import assessments.*

/** A [[SolutionElement]] that shows static explanation text. Created via `explain(...)` in
 * [[assessments.DynexiteDefaults]]. It awards no points and produces plain static HTML; it is
 * omitted from blank question sheets (`showSolutions = false`). For a solution element that also
 * scores, see [[GradingElement]]. */
class ExplanationElement(text: InterpolatedMarkdown[HtmlConvertible]) extends StaticElement, SolutionElement {
  lazy val html: Html = text.toHtml.flatMapArgs(_.toHtml)

  override def renderHtml(context: RenderContext, associatedFiles: FileMapBuilder): Html =
    if (!context.getOrElse(RenderContext.showSolutions, true))
      Html("")
    else
      Html(s"""<div class="explanation"><div class="explanation-body">${html.html}</div></div>""")
}
