package assessments

import org.scalatest.funsuite.AnyFunSuiteLike

class PlaintextTest extends AnyFunSuiteLike {
  test("toMarkdown leaves plain text unchanged") {
    assert(Plaintext("Stabilizer State").toMarkdown == Markdown("Stabilizer State"))
  }

  test("toMarkdown escapes markdown emphasis markers") {
    assert(Plaintext("a*b_c").toMarkdown == Markdown("a\\*b\\_c"))
    assert(Plaintext("**bold**").toMarkdown == Markdown("\\*\\*bold\\*\\*"))
  }

  test("toMarkdown escapes every ASCII punctuation character") {
    val punctuation = """!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~"""
    val expected = punctuation.flatMap(c => s"\\$c")
    assert(Plaintext(punctuation).toMarkdown == Markdown(expected))
  }

  test("toMarkdown does not escape letters, digits or spaces") {
    val text = "Hello 123 world"
    assert(Plaintext(text).toMarkdown == Markdown(text))
  }

  test("toMarkdown on empty string is empty") {
    assert(Plaintext("").toMarkdown == Markdown(""))
  }
}
