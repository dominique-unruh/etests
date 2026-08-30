package assessments.pageelements

import assessments.*

/** A [[SolutionElement]] that shows static explanation text. Created via `explain(...)` in
 * [[assessments.DynexiteDefaults]]. It awards no points and produces plain static HTML; it is
 * omitted from blank question sheets (`showSolutions = false`). Interpolated args may be plain
 * [[HtmlConvertible]] values or nested [[StaticElement]]s (rendered with the current context). For a
 * solution element that also scores, see [[GradingElement]]. */
class ExplanationElement(text: InterpolatedMarkdown[StaticElement | HtmlConvertible]) extends StaticElement, SolutionElement {
  override def renderHtml(context: RenderContext, associatedFiles: FileMapBuilder): Html =
    if (!context.getOrElse(RenderContext.showSolutions, true))
      Html("")
    else {
      val body = text.toHtml.flatMapArgs {
        case e: StaticElement => e.renderHtml(context, associatedFiles)
        case h: HtmlConvertible => h.toHtml
      }
      Html(s"""<div class="explanation"><div class="explanation-body">${body.html}</div></div>""")
    }
}
