package externalsystems

import externalsystems.MoodleStack.FunctionDefinition

/** Reusable validators for [[MoodleStack.moodleValidation]].
 *
 * Each validator is a [[MoodleStack.FunctionDefinition]] naming a Maxima function that takes the
 * student's answer and returns a validation string (empty = valid; non-empty = error message shown
 * to the student). When assigned to an input's [[MoodleStack.moodleValidation]] tag, the input gets
 * the extra option `validator:<name>` and the definitions are injected into the question variables.
 * Definitions are deduplicated at export time, so the same validator (or a shared helper) can be
 * used by several inputs without clashing. */
object MoodleValidationHelpers {
  /** Accepts only a concrete, real-valued formula, i.e. one that evaluates to a real number with
   * no free variables (e.g. `1/4`, `sqrt(2)`, `pi`), rejecting variables, non-numeric expressions
   * (e.g. `ket(0)`), and anything with an imaginary part (e.g. `2+3*i`).
   *
   * Binds `simp:true` locally because STACK evaluates question variables with `simp:false`, under
   * which `float`/`realpart` etc. stay unsimplified and the numeric checks would misfire. */
  val concreteReal: FunctionDefinition = FunctionDefinition(
    name = "concretereal",
    dependencies = Seq.empty,
    definitions = Seq(
      """concretereal(ans) := block([simp:true, v, r, re, im],
        |  v : errcatch(float(rectform(ans))),
        |  if emptyp(v) then "Please enter a value that evaluates to a concrete number."
        |  else (
        |    r : first(v), re : realpart(r), im : imagpart(r),
        |    if not (numberp(re) and numberp(im)) then "Please enter a real number (without variables)."
        |    elseif not is(equal(im, 0)) then "Please enter a real number (no imaginary part)."
        |    else ""
        |  )
        |);""".stripMargin))

  /** Validator for a well-typed superposition of `dim`-qubit computational basis states: every
   * `ket(...)` must be applied to a single `dim`-bit string (`ket(00...0)`, ..., `ket(11...1)`;
   * parsed as the integers whose decimal digits are the bits, e.g. `ket(010)` -> `10`), and the
   * whole expression must evaluate to a `2^dim`-dimensional vector (assuming `ket(...)` is such a
   * vector). Rejects bad ket arguments (`ket(2)`, too many/few digits, several args) and non-vector
   * expressions (a scalar, a bare variable, a dot product). `ketVector(3)` is the 3-qubit case. */
  def ketVector(dim: Int): FunctionDefinition = {
    require(dim >= 1, s"ketVector dimension must be >= 1: $dim")
    val name = s"ket${dim}vector"
    val vectorDim = 1 << dim
    // Valid ket labels: each dim-bit string read as a decimal integer (binary of b, leading zeros drop
    // to the same integer, matching how STACK parses ket(010) as 10).
    val validLabels = (0 until vectorDim).map(Integer.toBinaryString).mkString(", ")
    val example = "0" * dim
    FunctionDefinition(
      name = name,
      dependencies = Seq.empty,
      definitions = Seq(
        s"""$name(ans) := block([simp:true, valid:[$validLabels], r, e],
           |  e : errcatch(ev(ans, ket = lambda([[a]],
           |        if length(a) = 1 and member(first(a), valid) then makelist(1, i, 1, $vectorDim) else bad),
           |        nouns, simp)),
           |  if emptyp(e) then "Please enter a superposition of kets."
           |  else (
           |    r : first(e),
           |    if not freeof(bad, r) then "Every ket must be applied to a $dim-bit string, e.g. ket($example)."
           |    elseif listp(r) and length(r) = $vectorDim then ""
           |    else "The expression must be a vector of dimension $vectorDim (a linear combination of kets)."
           |  )
           |);""".stripMargin))
  }


  /** Maxima `texput` definition that renders a single-argument `ket(x)` as `\(|x\rangle\)`. Add it to
   * [[MoodleStack.moodleQuestionVariables]] so STACK displays ket-notation answers in bra-ket form.
   * The argument is shown as-is; use [[texSingleArgKet]] when all kets have the same fixed number of qubits and
   * you want leading zeros. */
  val texSingleArgKet = """texput(ket, lambda([ex], sconcat("\\lvert ", tex1(first(ex)), "\\rangle")))"""

  /** Like [[texSingleArgKet]], but renders the ket argument as a fixed-width `dim`-digit bit string,
   * zero-padded on the left (so `ket(10)` displays as `\(|010\rangle\)` for `dim = 3`). Use only for
   * questions whose kets all have exactly `dim` qubits (padding a differently-sized ket would mislabel
   * it).
   *
   * Pads with a `while`/`sconcat` loop rather than `printf`: STACK's CAS security forbids `printf`
   * ("Forbidden function"), which makes the whole question variables fail ("Error(s) in
   * question-variables"). */
  def texSingleArgKet(dim: Int): String =
    // `\\\\rangle` (not `\\rangle`): the `s` interpolator applies escape processing (unlike the plain
    // triple-quoted no-arg `texSingleArgKet`), so four backslashes are needed to emit the two that
    // Maxima's string literal needs to render a single `\rangle`.
    s"""texput(ket, lambda([ex], block([s: sconcat(first(ex))], while slength(s) < $dim do s: sconcat("0", s), sconcat("\\\\lvert ", s, "\\\\rangle"))))"""

}
