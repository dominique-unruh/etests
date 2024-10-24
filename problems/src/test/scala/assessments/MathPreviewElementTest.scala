package assessments

import assessments.MathPreviewElement.mathtextToLatex
import org.scalatest.funsuite.AnyFunSuiteLike

class MathPreviewElementTest extends AnyFunSuiteLike {
  test("mathtextToLatex") {
    mathtextToLatex("x**(1+1)")
  }
}
