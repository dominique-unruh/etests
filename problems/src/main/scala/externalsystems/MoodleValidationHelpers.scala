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

  /** Accepts only a concrete, complex-valued formula, i.e. one that evaluates to a complex number
   * with no free variables (e.g. `1/4`, `sqrt(2)`, `2+3*i`, `i`), rejecting variables and non-numeric
   * expressions (e.g. `ket(0)`). Unlike [[concreteReal]] it also accepts a non-zero imaginary part.
   *
   * Binds `simp:true` locally because STACK evaluates question variables with `simp:false`, under
   * which `float`/`realpart` etc. stay unsimplified and the numeric checks would misfire. */
  val concreteComplex: FunctionDefinition = FunctionDefinition(
    name = "concretecomplex",
    dependencies = Seq.empty,
    definitions = Seq(
      """concretecomplex(ans) := block([simp:true, v, r, re, im],
        |  v : errcatch(float(rectform(ans))),
        |  if emptyp(v) then "Please enter a value that evaluates to a concrete number."
        |  else (
        |    r : first(v), re : realpart(r), im : imagpart(r),
        |    if not (numberp(re) and numberp(im)) then "Please enter a complex number (without variables)."
        |    else ""
        |  )
        |);""".stripMargin))

  /** Lifts an element validator to a *set* validator: accepts a Maxima set `{...}` all of whose
   * elements pass `inner` (e.g. `setOf(concreteReal)` accepts `{-1, 1}`, `{1/2, sqrt(2)}`, rejects a
   * non-set like `5` and a set with a bad element like `{1, x}`, reporting the first element's error).
   * The empty set passes; pair with [[MoodleStack.MoodleExtraOptions.allowEmpty]] if that should be a
   * valid answer. The inner validator is added as a dependency, so its definitions are injected too.
   *
   * No `simp:true` here (unlike the numeric validators): `setp`/`listify` are structural and `inner`
   * binds its own `simp` locally as needed. */
  def setOf(inner: FunctionDefinition): FunctionDefinition = {
    val name = s"setof${inner.name}"
    FunctionDefinition(
      name = name,
      dependencies = Seq(inner),
      definitions = Seq(
        // Prefix the element's own error with which element failed, so the message doesn't read as if
        // the whole answer must be that thing (e.g. `{1, i}` -> "Set element \(i\): Please enter a real
        // ...", not a bare "Please enter a real number"). No "invalid" prefix here: STACK already
        // prepends "This answer is invalid." to the returned message. The element is shown as rendered
        // LaTeX via `\(tex1(e)\)` (STACK feedback runs through MathJax): this both looks right and sidesteps
        // Maxima's internal spelling of the imaginary unit (`string(i)` would print `%i`; `tex1` gives `i`).
        // `\\\\(` in this `s"""..."""` -> Maxima source `\\(` -> one literal backslash in the string.
        s"""$name(ans) := block([l, e, msg, r],
           |  if not setp(ans) then "Please enter a set, e.g. {1, 2}."
           |  else (
           |    l : listify(ans), msg : "",
           |    for e in l do (if msg = "" then (
           |      r : ${inner.name}(e),
           |      if r # "" then msg : sconcat("Set element \\\\(", tex1(e), "\\\\): ", r))),
           |    msg
           |  )
           |);""".stripMargin))
  }

  /** Validator for a well-typed superposition of `dim`-qubit computational basis states: every
   * `ket(...)` must be applied to a single `dim`-symbol label, and the whole expression must evaluate
   * to a `2^dim`-dimensional vector (assuming `ket(...)` is such a vector). Rejects bad ket arguments
   * (`ket(2)`, `ket(1/2)`, several args) and non-vector expressions (a scalar, a bare variable, a dot
   * product). `ketVector(3)` is the 3-qubit computational-basis case.
   *
   * With `variables` non-empty each label position may also hold one of those symbols (each standing
   * for a bit), so the labels are no longer purely numeric — e.g. `ketVector(3, Seq("a", "b"))` accepts
   * `ket(ab0)`, `ket(a0b)`, `ket(110)`, `ket(001)+ket(110)`, `1/sqrt(2)*(ket(ab0)-(-1)^a*ket(ab1))`.
   * A finite variable list and fixed `dim` give a finite set of possible labels, so we keep the
   * enumeration approach: every length-`dim` string over `{0, 1} ∪ variables` is enumerated and its
   * parsed Maxima form (a decimal integer for all-digit labels, a symbol otherwise) put in `valid`;
   * `ket(label)` is then accepted iff its argument is `member` of that list.
   *
   * Two subtleties: (1) Maxima parses a numeric ket label as an integer and drops leading zeros
   * (`ket(001)` becomes `ket(1)`), so `ket(001)` and `ket(1)` are the same — the enumerated integers
   * collapse the same way, and `[[texSingleArgKet]](dim)` re-pads for display. A consequence is that a
   * purely numeric shorter label such as `ket(0)`/`ket(1)` also validates here (it collapses onto
   * `000`/`001`), which conveniently also allows one-qubit partial answers. (2) Only labels whose
   * string is either all-digit or starts with a letter are enumerable as a single Maxima atom; a
   * digit-then-letter label (`0ab`) is not a single identifier and is dropped (STACK would not parse it
   * as one symbol either). */
  def ketVector(dim: Int, variables: Seq[String] = Seq.empty): FunctionDefinition = {
    require(dim >= 1, s"ketVector dimension must be >= 1: $dim")
    require(variables.forall(v => v.length == 1 && v.head.isLetter && v == v.toLowerCase),
      s"ketVector variables must be single lowercase letters: $variables")
    val vectorDim = 1 << dim
    val alphabet = Seq("0", "1") ++ variables
    // All length-dim labels over the alphabet, each mapped to how STACK parses it: an integer for a
    // pure-digit label (leading zeros collapse, e.g. 010 -> 10), the symbol itself for one starting
    // with a letter, dropped otherwise (a digit-then-letter string is not a single Maxima atom).
    val combos = Seq.fill(dim)(alphabet).foldRight(Seq(""))((opts, acc) => opts.flatMap(o => acc.map(o + _)))
    val validLabels = combos.flatMap { s =>
      if (s.forall(_.isDigit)) Some(BigInt(s).toString)
      else if (s.head.isLetter) Some(s)
      else None
    }.distinct.mkString(", ")
    val name = if (variables.isEmpty) s"ket${dim}vector" else s"ket${dim}vector${variables.mkString}"
    val example = (variables ++ Seq.fill(dim)("0")).take(dim).mkString
    val labelDesc =
      if (variables.isEmpty) s"a $dim-bit string"
      else s"a $dim-symbol string over {${alphabet.mkString(", ")}}"
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
           |    if not freeof(bad, r) then "Every ket must be applied to $labelDesc, e.g. ket($example)."
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
