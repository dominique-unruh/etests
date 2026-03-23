package utils

import org.scalatest.funsuite.AnyFunSuiteLike
import os.Path
import utils.LaTeX.latexToPng

import java.awt.image.BufferedImage
import java.io.{ByteArrayInputStream, File}
import java.nio.file.Files
import javax.imageio.ImageIO
import javax.swing.{ImageIcon, JLabel, JOptionPane}
import scala.sys.process.given

class LaTeXTest extends AnyFunSuiteLike {
  test("create PDF") {
    val source =
      raw"""\documentclass{article}
           |\begin{document}
           |Hello
           |\end{document}
           |""".stripMargin
    val pdf = LaTeX.latexToPDF(source)
  }
}

object LaTeXTest {
  def showPngImage(bytes: Array[Byte]): Unit = {
    val inputStream = new ByteArrayInputStream(bytes)
    val image: BufferedImage = ImageIO.read(inputStream)
    val icon = new ImageIcon(image)
    val label = new JLabel(icon)

    JOptionPane.showMessageDialog(null, label, "PNG Image", JOptionPane.PLAIN_MESSAGE)
  }

  def showPDF(bytes: Array[Byte]): Unit = {
    val tmp = File.createTempFile("document", ".pdf")
    tmp.deleteOnExit()
    Files.write(tmp.toPath, bytes)
    println(Seq("evince", tmp.toString).!!)
  }

  // Example usage
  def main(args: Array[String]): Unit = {
    val image = Path("/home/unruh/cloud/qis/shared/agnes-uvalic/templates-and-design/institute-logo/02_bildmarke_und_text_EN/pdf/rwth_lehrstuhl_quanteninformationssysteme_en_rgb.pdf")
    val source =
      raw"""\documentclass{article}
           |\usepackage{graphicx}
           |\begin{document}
           |Hello
           |
           |\includegraphics[width=\linewidth]{image.pdf}
           |
           |\end{document}
           |""".stripMargin
    try {
      val pdf = LaTeX.latexToPDF(source, Map("image.pdf" -> Files.readAllBytes(image.wrapped)))
      showPDF(pdf)
    } catch {
      case e: LaTeXException =>
        println(e.fileString("latex.log"))
        throw e
    }
  }
}
