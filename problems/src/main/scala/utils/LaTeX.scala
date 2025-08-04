package utils

import Docker.runInDocker
import utils.{IndentedInterpolator, Utils}

import java.awt.image.BufferedImage
import java.io.*
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Paths, StandardCopyOption}
import java.util.UUID
import javax.imageio.ImageIO
import javax.swing.{ImageIcon, JLabel, JOptionPane}
import scala.sys.process.*
import scala.util.{Failure, Success, Try}

object LaTeX {
  case class ConversionResult(success: Boolean, outputPath: Option[String], error: Option[String])

  object Preambles {
    val standard = "\\usepackage[T1]{fontenc}\n\\usepackage[utf8]{inputenc}\n\\usepackage{amsmath,amssymb}"
  }

  def latexToPng(latex: String, preamble: String = Preambles.standard): Array[Byte] = {
    val document =
      ind"""\\documentclass[tikz,border=2mm]{standalone}
           |$preamble
           |\\begin{document}
           |$latex
           |\\end{document}"""

    val script =
      """#!/bin/bash
        |set -ex
        |
        |echo "Starting LaTeX compilation..."
        |pdflatex -interaction=batchmode latex.tex
        |echo "LaTeX compilation successful"
        |magick -density 300 latex.pdf result.png &> convert.log
        |echo "Conversion completed successfully"
        |""".stripMargin

    val dockerResult = runInDocker(
      image = "aergus/latex:latest",
      command = Seq("/bin/bash", "script.sh"),
      files = Map("script.sh" -> script, "latex.tex" -> document),
      requestedOutputs = Seq("result.png", "latex.log", "convert.log")
    )

    if (dockerResult.exitCode != 0) {
      (dockerResult.fileString("latex.log"), dockerResult.fileString("convert.log")) match {
        case (None, None) => throw IOException(s"Failed to run latex.\n$document")
        case (Some(latexLog), None) => throw IOException(s"Failed to run latex.\n$document\n$latexLog")
        case (_, Some(convertLog)) => throw IOException("Failed to convert PDF to PNG.\n"+convertLog)
      }
    }

    dockerResult.files("result.png")
  }

  def showPngImage(bytes: Array[Byte]): Unit = {
    val inputStream = new ByteArrayInputStream(bytes)
    val image: BufferedImage = ImageIO.read(inputStream)
    val icon = new ImageIcon(image)
    val label = new JLabel(icon)

    JOptionPane.showMessageDialog(null, label, "PNG Image", JOptionPane.PLAIN_MESSAGE)
  }

  // Example usage
  def main(args: Array[String]): Unit = {
    // Ensure Docker image is available

    val tikzCode = """
                     |\begin{tikzpicture}[scale=2]
                     |  \draw[thick,->] (0,0) -- (2.2,0) node[anchor=north west] {$x$};
                     |  \draw[thick,->] (0,0) -- (0,2.2) node[anchor=south east] {$y$};
                     |
                     |  \draw[blue,thick,domain=0:2,samples=100] plot (\x,{sin(\x r)});
                     |  \draw[red,thick,domain=0:2,samples=100] plot (\x,{cos(\x r)});
                     |
                     |  \node[blue] at (1.5,1.5) {$\sin(x)$};
                     |  \node[red] at (0.5,1.5) {$\cos(x)$};
                     |  \x
                     |\end{tikzpicture}""".stripMargin

    val result = latexToPng(tikzCode)

    showPngImage(result)

  }
}

