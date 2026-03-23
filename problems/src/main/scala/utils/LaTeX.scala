package utils

import Docker.runInDocker
import com.typesafe.scalalogging.Logger
import utils.Utils.awaitResult
import utils.{IndentedInterpolator, Utils}

import java.awt.image.BufferedImage
import java.io.*
import java.nio.charset.StandardCharsets.UTF_8
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

  def latexToPDF(document: String, files: Map[String, Array[Byte]] = Map.empty): Array[Byte] = {
    val script =
      """#!/bin/bash
        |set -ex
        |
        |echo "Starting LaTeX compilation..."
        |pdflatex -halt-on-error -interaction=batchmode latex.tex
        |echo "Compiled successfully"
        |""".stripMargin

    val dockerResult = runInDocker(
      image = "aergus/latex:latest",
      command = Seq("/bin/bash", "script.sh"),
      files = Map("script.sh" -> script, "latex.tex" -> document) ++ files,
      requestedOutputs = Seq("latex.pdf", "latex.log")
    ).awaitResult()

    if (dockerResult.exitCode != 0) {
      dockerResult.fileString("latex.log") match {
        case None =>
          throw LaTeXException(s"Failed to run LaTeX (no log file produced)", Map ("latex.tex" -> document.getBytes(UTF_8)))
        case Some(latexLog) =>
          throw LaTeXException(s"Failed to run latex", Map ("latex.tex" -> document.getBytes(UTF_8), "latex.log" -> latexLog.getBytes(UTF_8)))
      }
    }

    dockerResult.files("latex.pdf")
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
    ).awaitResult()

    if (dockerResult.exitCode != 0) {
      (dockerResult.fileString("latex.log"), dockerResult.fileString("convert.log")) match {
        case (None, None) =>
          throw LaTeXException(s"Failed to run LaTeX (no log file produced)", Map ("latex.tex" -> document.getBytes(UTF_8)))
        case (Some(latexLog), None) =>
          throw LaTeXException(s"Failed to run latex", Map ("latex.tex" -> document.getBytes(UTF_8), "latex.log" -> latexLog.getBytes(UTF_8)))
        case (_, Some(convertLog)) =>
          throw LaTeXException("Failed to convert PDF to PNG", Map ("latex.tex" -> document.getBytes(UTF_8), "convert.log" -> convertLog.getBytes(UTF_8)))
      }
    }

    dockerResult.files("result.png")
  }

  private val escapes = Map(
    '\\' -> "\\textbackslash{}",
    '{' -> "\\{",
    '}' -> "\\}",
    '$' -> "\\$",
    '&' -> "\\&",
    '%' -> "\\%",
    '#' -> "\\#",
    '_' -> "\\_",
    '^' -> "\\textasciicircum{}",
    '~' -> "\\textasciitilde{}",
    '<' -> "\\textless{}",
    '>' -> "\\textgreater{}",
  )

  /** Escapes a plaintext string as LaTeX */
  def escape(string: String): String =
    string.flatMap(c => escapes.getOrElse(c, c.toString))
  
  private val logger = Logger[LaTeX.type]
}

case class LaTeXException(message: String, files: Map[String, Array[Byte]]) extends IOException(message) {
  def fileString(name: String): String = String(files(name), UTF_8)
}
