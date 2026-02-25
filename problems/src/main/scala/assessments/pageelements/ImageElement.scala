package assessments.pageelements

import assessments.ExceptionContext.addToExceptionContext
import assessments.{Assessment, ExceptionContext, ExceptionWithContext, FileMapBuilder, Html}
import assessments.pageelements.StaticElement
import org.apache.batik.transcoder.{SVGAbstractTranscoder, TranscoderInput, TranscoderOutput}
import org.apache.batik.transcoder.image.PNGTranscoder
import org.apache.commons.text.StringEscapeUtils.escapeHtml4
import sourcecode.Enclosing

import java.io.{ByteArrayOutputStream, InputStream, Reader}

case class ImageElement(png: Array[Byte], basename: String) extends StaticElement {
  override def renderHtml(associatedFiles: FileMapBuilder): Html = {
    val name = associatedFiles.add(basename = basename, extension = "png", mimeType = "image/png", content = png)
    Html(s"""<img src="${escapeHtml4(name)}"/>""")
  }
}

object ImageElement {
  def fromSVGResource(svgResource: String, clazz: Class[?])(using enclosing: Enclosing): ImageElement = {
//    given ExceptionContext = addToExceptionContext(s"Loading SVG image $svgResource")
    val stream = clazz.getResourceAsStream(svgResource)
    if (stream == null)
      throw RuntimeException(s"Resource $svgResource not found when trying to load an SVG from it (relative to class ${clazz.getName}).")
//      throw new ExceptionWithContext(s"Resource $svgResource not found.")
    fromSVG(stream, basename = svgResource.split('/').last.stripSuffix(".svg"))
  }
  def fromSVG(svg: InputStream, basename: String): ImageElement = {
    val transcoder = new PNGTranscoder
    transcoder.addTranscodingHint(SVGAbstractTranscoder.KEY_WIDTH, 1000.toFloat)
    val input = new TranscoderInput(svg)
    val outputStream = new ByteArrayOutputStream
    val output = new TranscoderOutput(outputStream)
    transcoder.transcode(input, output)
    output.getOutputStream.flush()
    output.getOutputStream.close()
    val png = outputStream.toByteArray
    ImageElement(png, basename)
  }
  def fromPNGResource(pngResource: String, clazz: Class[?])(using enclosing: Enclosing): ImageElement = {
    val stream = clazz.getResourceAsStream(pngResource)
    if (stream == null)
      throw RuntimeException(s"Resource $pngResource not found when trying to load a PNG from it (relative to class ${clazz.getName}).")
    ImageElement(stream.readAllBytes(), pngResource.split('/').last.stripSuffix(".png"))
  }
}
