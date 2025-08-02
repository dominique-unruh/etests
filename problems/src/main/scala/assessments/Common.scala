package assessments

import assessments.pageelements.ImageElement
import externalsystems.LaTeX
import externalsystems.LaTeX.Preambles
import utils.Utils

object Common {
  def latexPreamble = raw"""${LaTeX.Preambles.standard}
\usetikzlibrary{arrows,shapes,positioning,shadows,trees,patterns,decorations}
\usepackage{quantikz}
"""

  def latex(code: String): ImageElement = { ImageElement(LaTeX.latexToPng(code, preamble = latexPreamble), "latexpicture") }
  def quantikz(code: String, options: String = "", extraPreamble: String = ""): ImageElement = {
    ImageElement(LaTeX.latexToPng(s"\\begin{quantikz}[$options]\n${Utils.stripLeadingTrailingEmptyLines(code)}\\end{quantikz}",
      preamble = latexPreamble+"\n"+extraPreamble), "quantikz")
  }
  def tikz(code: String, options: String = "", extraPreamble: String = ""): ImageElement = {
    ImageElement(LaTeX.latexToPng(s"\\begin{tikzpicture}[$options]\n${Utils.stripLeadingTrailingEmptyLines(code)}\\end{tikzpicture}",
      preamble = latexPreamble+"\n"+extraPreamble), "tikz")
  }
}
