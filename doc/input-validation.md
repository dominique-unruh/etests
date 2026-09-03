# Input validation (STACK validators)

STACK can run a *validator* on a student's answer before it is graded: a Maxima function that
inspects the entered value and either accepts it or shows an error message ("This answer is
invalid: ..."). This is validation, not grading — it only decides whether the input is well-formed
enough to be submitted. Use it to reject nonsense early (e.g. a complex number where a real number
is expected).

This is wired up for the Moodle/STACK export in
`problems/src/main/scala/externalsystems/MoodleStack.scala`.

## Attaching a validator to an input

Give the input element the `moodleValidation` tag, whose value is a
`MoodleStack.FunctionDefinition` (see below):

```scala
import externalsystems.MoodleStack.moodleValidation
import externalsystems.MoodleValidationHelpers

val prob = input("1/4",
  Tags(moodleAllowWords := Seq("ket", "i", "pi", "e"),
    moodleValidation := MoodleValidationHelpers.concreteReal))
```

`MoodleValidationHelpers` holds ready-made validators. Currently:

* `concreteReal` — accepts only a concrete, real-valued formula: something that evaluates to a real
  number with no free variables (`1/4`, `sqrt(2)`, `pi`, `sin(1)` are accepted; `2+3*i`, `%i`,
  `sqrt(-1)` are rejected as non-real; `x`, `ket(0)` are rejected as non-numeric / containing
  unknowns).

## The validator contract

A validator is a Maxima function of one argument (the student's answer). It returns a **string**:

* the empty string `""` means the answer is valid;
* any non-empty string is the error message shown to the student.

## `FunctionDefinition`

`MoodleStack.FunctionDefinition` bundles a named Maxima function with the definitions it needs:

```scala
case class FunctionDefinition(name: String, helpers: Seq[FunctionDefinition], defs: Seq[String]) {
  def allDefs: Seq[String] = helpers.flatMap(_.allDefs) ++ defs
}
```

* `name` — the Maxima function name. It is exported as the input's extra option `validator:<name>`.
* `defs` — the Maxima definitions of this function (one definition per string; must include the one
  defining `name`).
* `helpers` — other `FunctionDefinition`s this one depends on. `allDefs` collects their definitions
  (transitively) and prepends them, so helper functions are defined before the functions using them.

Example:

```scala
val concreteReal: FunctionDefinition = FunctionDefinition(
  name = "concreteReal",
  helpers = Seq.empty,
  defs = Seq(
    """concreteReal(ans) := block([simp:true, v, r, re, im],
      |  v : errcatch(float(rectform(ans))),
      |  if emptyp(v) then "Please enter a value that evaluates to a concrete number."
      |  else (
      |    r : first(v), re : realpart(r), im : imagpart(r),
      |    if not (numberp(re) and numberp(im)) then "Please enter a concrete value (an expression without unknowns)."
      |    elseif not is(equal(im, 0)) then "Please enter a real-valued expression (no imaginary part)."
      |    else ""
      |  )
      |);""".stripMargin))
```

## How it is exported

`inputElementToMoodle` turns the tag into two things:

1. the extra option `validator:<name>` on the input (STACK's "Extra options" field); and
2. the function's `allDefs`, contributed to the question variables.

At the question level (`Question.xml`) these per-input definitions are merged with the question's own
`moodleQuestionVariables`:

* identical definition strings are **deduplicated**, so a helper shared by several inputs (or the
  same validator used on several inputs) is emitted only once and does not clash; then
* if two *different* definitions define the same variable/function name, export **fails** with an
  error (a genuine conflict). The defined name is detected from the `name : ...` / `name(args) := ...`
  shape of each definition.

Because of this, `moodleQuestionVariables` is a `Seq[String]` (one definition per element), not a
single blob.

Each definition is emitted on its own line, and the export appends a terminating `;` to any
definition that does not already end in `;` or `$`. (Maxima separates statements with `;`/`$`; a
definition lacking one would glue onto the next and cause a syntax error — see below.)

## Gotcha: "the optional validator threw internal Maxima errors"

STACK shows this generic message (lang string `inputvalidatorerrcouldnot`) whenever the validator
call does not evaluate to a string — most often because the function is *missing*, so the call comes
back as an unevaluated noun. Causes we have hit:

* **Mixed-case validator name.** STACK is case-insensitive and lowercases identifiers when it
  resolves the `validator:` option, so a name like `concreteReal` becomes the call `concretereal(...)`
  while the question-variable definition keeps its original case — the two never match and the call
  stays unevaluated. **Validator function names must be lowercase.** `inputElementToMoodle` enforces
  this with a `require`.
* **Unterminated question variables.** If a question variable (e.g. a `texput(...)` line) had no
  trailing `;`, the validator definition that followed it was glued on, the question variables failed
  to parse (`... is not an infix operator`), and the validator function was never defined — so
  calling it errored. The export now auto-terminates each definition (see above), which fixes this.
* **`simp:false`.** See below.

The stack-parser service now logs the real cause to stderr when a validator fails, e.g.
`[stack validator] option "concretereal" on input "prob" did not return a string; got:
concretereal(1) (is the validator function defined, with a lowercase name?)` — see
`stack/input/inputbase.class.php`.

* **`/parse` did not instantiate the question.** The webapp's live preview validates one input via the
  service's `/parse` endpoint (`api/public/parseservice.php`). It used to call
  `validate_student_response` on the input in isolation, without the compiled question variables — so a
  validator whose function is defined in the question variables was undefined. Fixed by instantiating
  the question (`initialise_question_from_seed`) and validating via `get_input_state`, which passes the
  question variables into the input's validation.

## Gotcha: `simp:false`

STACK evaluates question variables (and therefore validator functions) with `simp:false`. Under
that setting, expressions like `float(rectform(ans))`, `realpart(...)`, `imagpart(...)` are left
**unsimplified**, so numeric tests such as `numberp(...)` and `is(equal(..., 0))` misfire — the
symptom is the student seeing *"This answer is invalid. The optional validator threw internal Maxima
errors."* even for a trivially valid input like `1`.

Fix: bind `simp:true` locally inside the validator's block, i.e.
`block([simp:true, ...], ...)`. `concreteReal` does this.

## Writing / testing your own validator

Add a new `FunctionDefinition` to `MoodleValidationHelpers` (or build one inline). Rules of thumb:
lowercase name; bind `simp:true`; guard evaluation with `errcatch`; return a string.

Two levels of local testing:

* **Quick Maxima check** (function logic only). A STACK-loaded Maxima lives in the optimised service
  image; run it with `simp:false` (STACK's setting):
  ```bash
  docker run --rm -i etests-stack-parser:opt \
    /moodle-qtype_stack/stack/maxima_opt_auto <<'EOF'
  simp:false$
  concretereal(ans) := block([simp:true, ...]);
  concretereal(1);
  EOF
  ```
* **Full STACK validation** (the real path, catches the case/parse issues above). Export the question
  (`extractStack`), then drive STACK's input validation in the `:opt` image via
  `qtype_stack_question::get_input_state(inputName, [inputName => answer])` +
  `render_validation(...)` — this mirrors the `/validate` (and `/parse`) endpoints the webapp uses.
  This is how the bugs above were reproduced.
