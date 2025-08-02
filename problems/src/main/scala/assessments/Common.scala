package assessments

import assessments.pageelements.ImageElement
import externalsystems.LaTeX
import externalsystems.LaTeX.Preambles
import utils.Utils

object Common {
  def latex(code: String): ImageElement = { ImageElement(LaTeX.latexToPng(code), "latexpicture") }
  def quantikz(code: String, options: String = "", extraPreamble: String = ""): ImageElement = {
    ImageElement(LaTeX.latexToPng(s"\\begin{quantikz}[$options]\n${Utils.stripLeadingEmptyLines(code)}\\end{quantikz}",
      preamble = Preambles.quantikz+"\n"+extraPreamble), "quantikz")
  }
}
