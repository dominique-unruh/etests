package assessments.pageelements

import assessments.{FileMapBuilder, Html}

case class HtmlElement(html: Html) extends StaticElement:
  override def renderHtml(associatedFiles: FileMapBuilder): Html = html
