package assessments.pageelements

import assessments.{FileMapBuilder, Html}

case class HtmlElement(html: Html) extends StaticElement:
  override def renderHtml(context: RenderContext, associatedFiles: FileMapBuilder): Html = html
