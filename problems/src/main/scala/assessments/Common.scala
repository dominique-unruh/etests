package assessments

import assessments.pageelements.{ErrorElement, ImageElement, StaticElement}
import utils.LaTeX.Preambles
import utils.{LaTeX, LaTeXException, Utils}

object Common {
  def latexPreamble = raw"""${LaTeX.Preambles.standard}
\usetikzlibrary{arrows,shapes,positioning,shadows,trees,patterns,decorations}
\usepackage{quantikz}
"""

  private def catchLatexError(element: => StaticElement): StaticElement = {
    try
      element
    catch
      case e: LaTeXException =>
        ErrorElement(e.toString, e.files)
  }

  def latex(code: String): StaticElement = catchLatexError { ImageElement(LaTeX.latexToPng(code, preamble = latexPreamble), "latexpicture") }
  def quantikz(code: String, options: String = "", extraPreamble: String = ""): StaticElement = catchLatexError {
    ImageElement(LaTeX.latexToPng(s"\\begin{quantikz}[$options]\n${Utils.stripLeadingTrailingEmptyLines(code)}\\end{quantikz}",
      preamble = latexPreamble+"\n"+extraPreamble), "quantikz")
  }
  def tikz(code: String, options: String = "", extraPreamble: String = ""): StaticElement = catchLatexError {
    ImageElement(LaTeX.latexToPng(s"\\begin{tikzpicture}[$options]\n${Utils.stripLeadingTrailingEmptyLines(code)}\\end{tikzpicture}",
      preamble = latexPreamble+"\n"+extraPreamble), "tikz")
  }
}
