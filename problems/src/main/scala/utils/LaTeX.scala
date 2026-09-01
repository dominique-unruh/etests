package utils

import Docker.runInDocker
import com.typesafe.scalalogging.Logger
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.*
import sttp.client4.*
import sttp.client4.circe.*
import utils.Utils.awaitResult
import utils.{IndentedInterpolator, Utils}

import java.io.*
import java.nio.charset.StandardCharsets.UTF_8
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

  private case class LatexToPdfRequest(document: String, files: Map[String, Array[Byte]])
  private object LatexToPdfRequest {
    given Encoder[LatexToPdfRequest] = {
      import utils.CirceCodecs.byteArrayEncoder
      deriveEncoder
    }
  }

  private case class LatexToPngRequest(latex: String, preamble: String)
  private object LatexToPngRequest { given Encoder[LatexToPngRequest] = deriveEncoder }

  /** Response from the latex HTTP service.
   * `result` is the compiled output (PDF or PNG bytes); null means compilation failed.
   * `latexLog` and `convertLog` are log files for error reporting. */
  private case class LatexResponse(result: Option[Array[Byte]], latexLog: Option[Array[Byte]], convertLog: Option[Array[Byte]])
  private object LatexResponse {
    given Decoder[LatexResponse] = {
      import utils.CirceCodecs.byteArrayDecoder
      deriveDecoder
    }
  }

  def latexToPDF(document: String, files: Map[String, Array[Byte]] = Map.empty): Array[Byte] = {
    Utils.getSystemPropertyOptional("latex.url",
        "base URL of the LaTeX HTTP service, e.g. http://localhost:8081; if unset, a one-shot Docker container is used per call") match {
      case Some(baseUrl) =>
        import LatexToPdfRequest.given
        import LatexResponse.given
        val response = basicRequest
          .post(uri"${baseUrl.stripSuffix("/")}/latex-to-pdf")
          .contentType("application/json")
          .body(LatexToPdfRequest(document, files).asJson.noSpaces)
          .response(asJson[LatexResponse])
          .send(backend)
          .awaitResult()
        response.body match {
          case Right(resp) =>
            resp.result.getOrElse {
              resp.latexLog match {
                case None =>
                  throw LaTeXException("Failed to run LaTeX (no log file produced)", Map("latex.tex" -> document.getBytes(UTF_8)))
                case Some(latexLog) =>
                  throw LaTeXException("Failed to run latex", Map("latex.tex" -> document.getBytes(UTF_8), "latex.log" -> latexLog))
              }
            }
          case Left(error) =>
            throw RuntimeException(s"latex service failed for PDF: ${error.getMessage}")
        }

      case None =>
        val script =
          """#!/bin/bash
            |set -ex
            |
            |echo "Starting LaTeX compilation..."
            |pdflatex -halt-on-error -interaction=batchmode latex.tex
            |echo "Compiled successfully"
            |""".stripMargin

        val dockerResult = runInDocker(
          shortDescription = "Latex to PDF",
          image = "docker.io/aergus/latex:latest",
          command = Seq("/bin/bash", "script.sh"),
          files = Map("script.sh" -> script, "latex.tex" -> document) ++ files,
          requestedOutputs = Seq("latex.pdf", "latex.log")
        ).awaitResult()

        if (dockerResult.exitCode != 0) {
          dockerResult.fileString("latex.log") match {
            case None =>
              throw LaTeXException(s"Failed to run LaTeX (no log file produced)", Map("latex.tex" -> document.getBytes(UTF_8)))
            case Some(latexLog) =>
              throw LaTeXException(s"Failed to run latex", Map("latex.tex" -> document.getBytes(UTF_8), "latex.log" -> latexLog.getBytes(UTF_8)))
          }
        }

        dockerResult.files("latex.pdf")
    }
  }

  def latexToPng(latex: String, preamble: String = Preambles.standard): Array[Byte] = {
    val document =
      ind"""\\documentclass[tikz,border=2mm]{standalone}
           |$preamble
           |\\begin{document}
           |$latex
           |\\end{document}"""

    Utils.getSystemPropertyOptional("latex.url",
        "base URL of the LaTeX HTTP service, e.g. http://localhost:8081; if unset, a one-shot Docker container is used per call") match {
      case Some(baseUrl) =>
        import LatexToPngRequest.given
        import LatexResponse.given
        val response = basicRequest
          .post(uri"${baseUrl.stripSuffix("/")}/latex-to-png")
          .contentType("application/json")
          .body(LatexToPngRequest(latex, preamble).asJson.noSpaces)
          .response(asJson[LatexResponse])
          .send(backend)
          .awaitResult()
        response.body match {
          case Right(resp) =>
            resp.result.getOrElse {
              (resp.latexLog, resp.convertLog) match {
                case (None, None) =>
                  throw LaTeXException("Failed to run LaTeX (no log file produced)", Map("latex.tex" -> document.getBytes(UTF_8)))
                case (Some(latexLog), None) =>
                  throw LaTeXException("Failed to run latex", Map("latex.tex" -> document.getBytes(UTF_8), "latex.log" -> latexLog))
                case (_, Some(convertLog)) =>
                  throw LaTeXException("Failed to convert PDF to PNG", Map("latex.tex" -> document.getBytes(UTF_8), "convert.log" -> convertLog))
              }
            }
          case Left(error) =>
            throw RuntimeException(s"latex service failed for PNG: ${error.getMessage}")
        }

      case None =>
        val script =
          """#!/bin/bash
            |set -ex
            |
            |echo "Starting LaTeX compilation..."
            |pdflatex -interaction=batchmode latex.tex
            |echo "LaTeX compilation successful"
            |magick -density 300 latex.pdf +set date:create +set date:modify -define png:exclude-chunks=date,tIME -strip result.png &> convert.log
            |echo "Conversion completed successfully"
            |""".stripMargin

        val dockerResult = runInDocker(
          shortDescription = "Latex to PNG",
          image = "docker.io/aergus/latex:latest",
          command = Seq("/bin/bash", "script.sh"),
          files = Map("script.sh" -> script, "latex.tex" -> document),
          requestedOutputs = Seq("result.png", "latex.log", "convert.log")
        ).awaitResult()

        if (dockerResult.exitCode != 0) {
          (dockerResult.fileString("latex.log"), dockerResult.fileString("convert.log")) match {
            case (None, None) =>
              throw LaTeXException(s"Failed to run LaTeX (no log file produced)", Map("latex.tex" -> document.getBytes(UTF_8)))
            case (Some(latexLog), None) =>
              throw LaTeXException(s"Failed to run latex", Map("latex.tex" -> document.getBytes(UTF_8), "latex.log" -> latexLog.getBytes(UTF_8)))
            case (_, Some(convertLog)) =>
              throw LaTeXException("Failed to convert PDF to PNG", Map("latex.tex" -> document.getBytes(UTF_8), "convert.log" -> convertLog.getBytes(UTF_8)))
          }
        }

        dockerResult.files("result.png")
    }
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
  private lazy val backend = DefaultFutureBackend()
}

case class LaTeXException(message: String, files: Map[String, Array[Byte]]) extends IOException(message) {
  def fileString(name: String): String = String(files(name), UTF_8)
}
