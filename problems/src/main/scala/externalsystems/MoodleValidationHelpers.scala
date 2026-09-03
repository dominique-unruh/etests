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
}
