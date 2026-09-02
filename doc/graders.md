# How to write grading rules?

Grading rules are written inline in the question text.
A single grading rule is a basic instruction for grading.
E.g., "the number above is correct up to a constant factor: 3 points".
A grading rule is added by writing
`${grading(text=md"""TEXT""", points=POINTS, grader=GRADER, name="NAME", partial=…, negative=…, unless=…)}`
inside the question markdown.
Here:
- `TEXT` is a human readable description of the grading rule (in markdown format).
- `POINTS` is the number of points of this rule (the full amount added/removed when it fires).
- `GRADER` is the function that decides, given the student's answers, **whether and how far** the rule
  fires (see "How to write graders?" below). It returns a `GraderOutcome`, not points.
- `NAME` is the name of the grading rule. Must be a valid identifier.
- `partial` (default `false`): whether this rule awards *partial* credit. Affects only the displayed
  badge (yellow instead of green/red), not the points. Set it on every partial-credit rule.
- `negative` (default `false`): whether this is a **penalty** rule — firing *removes* points instead
  of adding them.
- `unless` (default empty): a `Seq[ElementName]` of other rules; if **any** of them fired (fully or
  partially), this rule is not evaluated at all and shows as `notApplicable`. Use it to express
  priority / mutual exclusion between rules (highest-priority rule listed in the lower ones' `unless`).

A problem can have many grading rules.
The grading rules are not shown to the student during the exam; but TEXT is shown after the exam.  

The position of the grading rule does not matter for the underlying grading logic,
however the position matters for the interpretation of the TEXT
(because it usually refers to the inputs above it). Evaluation order follows `unless`
(topologically), not source position; a cycle in `unless` is an error.

# How to write graders?

For readability, only elementary graders should be directly included in the `grading(...)` call.
Elementary graders include:
- `missingGrader`: To indicate that a grader is still to be added.
- `equalsReference`: Gives full points if the student solution for a given input field matches the reference, 0 otherwise.

Otherwise each grader should be placed in files in the subpackages `graders` (inside the package where the problem is).
Grader functions for problem object `X` should be in the object `graders.XGrader` and imported in the problem's file.
Helper functions can be placed in the same file, or, if shared between problems, in another file in the `graders` package.
Helper functions must not be shared between different exams!

A grader is a block that returns a **`GraderOutcome`**. It does **not** award points directly; the
points follow from the outcome together with the rule's `points` and `negative` flag:
- `fires` — the rule applies fully. Adds the rule's full `points` (or removes them if `negative`).
- `firesPartially(p)` — the rule applies partially. Adds/removes `p` points, where `0 < p < points`
  (otherwise the grader throws). Use this when a *single* rule gives a fraction of its own points;
  most rules just use `fires` and let a separate lower rule handle the smaller-credit case.
- `doesntFire` — the rule does not apply. Zero points.

A rule that a higher-priority rule pre-empts is **not** something the grader has to signal: list the
higher rule(s) in this rule's `unless`, and when they fire this rule is automatically skipped and shown
as `notApplicable`. So a grader only ever decides `fires` / `firesPartially` / `doesntFire` for *its
own* case; it never returns `notApplicable` or a "missing"/"incorrect" verdict (an empty or wrong
answer for this rule's case is simply `doesntFire`).

**Full vs. partial credit is a property of the rule, not the outcome.** Whether a fired rule is "the
answer is fully correct" (green badge) or "partial credit" (yellow badge) is set by the `partial` flag
on `grading(...)`, decoupled from the grader. Mark every partial-credit rule (`correct up to sign`,
`correct but not normalized`, `any quantum state`, a not-penalized follow-up mistake, …) with
`partial = true`; leave the single fully-correct rule as `partial = false`.

A grader can additionally produce a number of comments.
These are shown to the students together with the points.
For simple cases, they are not necessary.

## Writing rules

### Misc

* Do `import assessments.GradingContext.*` and
  `import assessments.GradingContext.GraderOutcome.{fires, firesPartially, doesntFire}` to get the
  features below.
* A grader **returns** `fires` / `firesPartially(p)` / `doesntFire`; it does not touch points.
* `max` is this rule's reachable points (useful for a `firesPartially` fraction).
* `comments += "hello"` adds a comment `hello` (interpreted as markdown)
* `answers(element) = ...` can **change** an answer given by the student
  (useful for doing some cleanup like trimming whitespaces or replacing special cases)
* The webapp badge (`DisplayOutcome`) is derived automatically from the outcome plus the rule's
  `partial`/`negative` flags: positive full fire → green ✓, any partial fire → yellow ◐, positive
  non-fire → red ✗, negative full fire (penalty applied) → red ✓, negative non-fire → green ✗,
  `unless`-suppressed → gray n/a. You do not set it.

### Don't use exception handlers

In a grader function, never use exception handlers (`... catch { ... }`).
These easily lead to hidden grading mistakes because a handler might 
initially have been intended to catch one problem (e.g., a student-caused syntax error)
and therefore deduct points, and then after subsequent edits (or due to programmer errors)
there is another cause for a similar exception where the student is not at fault and 
gets points deducted unfairly because the same handler catches it.

Because of that, we simply forbid exception handlers completely.
**Throwing** exceptions is allowed but if these get triggered by student answers,
we need to add extra checks (not handlers) in order not to throw them.
Basically, an exception denotes a to-do that can be done if it becomes necessary.

How do avoid exception handlers? Example: If you have a function `parse(string)`
that throws a syntax error, and you have the code:
```scala 3
...
try
  if (checkSomeMathProperty(parse(string))) fires else doesntFire
catch
  case _ : SyntaxError => doesntFire
...
```
then you need to define, e.g., a helper function 
`def safeParse(string) = try Some(parse(string)) catch e: SyntaxError => None`
or similar (better: in a way that produces feedback that can be given to the student).
And then in the grader itself you do
```scala 3
val parsed = safeParse(string)
...
if (parsed != None && checkSomeMathProperty(parsed.get)) fires else doesntFire
...
```

# Testing graders

Graders are tested from **inside the problem file** (not in a separate test source). The tests are
ordinary calls in the body of the `MarkdownAssessment` object; they register test cases that run under
`sbt test`. They are useful even before a grader is implemented: with the grader still `missingGrader`,
the tests document the intended behavior and fail until the grader is written.

## Test solutions

A test runs a grader against a *solution*, i.e. an `Answers` value assigning a string to each input
field. Build them from `referenceSolution` by overriding individual fields:

```scala 3
lazy val solHalf = referenceSolution.update(ans1 -> "1/2")            // one field changed
lazy val solTwoFields = referenceSolution.update(field1 -> "a", field2 -> "b")
```

Two solutions are **predefined** and always available — do **not** redeclare them:
- `referenceSolution`: every field at its reference answer (the fully-correct solution).
- `emptySolution`: every field empty. Use this to test the "missing answer" case; there is no need to
  define an empty solution yourself.

Every `val`/`lazy val` of type `Answers` you declare in the object is automatically picked up as a
test solution by the group/overall tests below (via reflection), alongside these two predefined ones.

Import the outcomes you assert with
`import assessments.GradingContext.GraderOutcome.{fires, firesPartially, doesntFire}`.

## `testGrader` — a single grading rule (in isolation)

```scala 3
testGrader(grader, solution, outcome = …)
```

Asserts that running the grader named `grader` on `solution` **in isolation** (ignoring `unless`)
yields the given `GraderOutcome` — one of `fires`, `firesPartially(p)`, `doesntFire`. `grader` is the
rule name (the `name="…"` you passed to `grading(...)`) or the grading element itself. There is no
`points` argument: the points follow from the outcome and the rule config, so asserting the outcome
already pins them down. Example:

```scala 3
testGrader("correctGrader", referenceSolution, fires)
testGrader("grader12", solHalf, fires)          // grader12's own case
testGrader("grader12", referenceSolution, doesntFire) // not grader12's case (in isolation)
testGrader("grader12", emptySolution, doesntFire)     // empty answer is just doesntFire
```

Because `testGrader` runs the grader in isolation, it never asserts `notApplicable` (that is an
`unless`-driven, whole-assessment verdict — see the reference-solution check and `testGraderGroup`).
An empty or wrong answer for a rule's case is simply `doesntFire`.

An optional `test = (html: Html) => …` argument runs an extra assertion on the rendered rule text
(including comments the grader added), e.g. to check a specific comment was produced.

Related: `testGraderThrows(grader, solution)` asserts that the grader *throws* on that solution (graders
throw to flag a case they deliberately do not handle — see above).

## `testGraderGroup` — mutual exclusion

```scala 3
testGraderGroup(name = "distinctGraders",
  graders = Seq("correctGrader", "grader12", "grader1"))
```

Asserts that **at most one** of the listed graders fires in the *actual* grading of the solution —
i.e. grading the whole assessment (which honors each rule's `unless`; a rule suppressed by an
`unless` counts as `notApplicable`, not fired). Use it for a priority chain of mutually-exclusive
rules: it checks that the rules **together with their `unless` wiring** never award two of them at
once — the real double-credit failure mode (a missing or wrong `unless`).

Unlike `testGrader` (which runs one grader in isolation), this runs the joint grading, so the
graders need **not** be mutually exclusive on their own. A lower rule may fire in isolation as long
as a higher rule's `unless` suppresses it here; the group only fails if two rules survive `unless`
and both fire. (It runs with exceptions **not** caught, so a `missingGrader`/throwing grader fails
the test.)

If `solution` is omitted (the default), the check runs **once for each** test solution —
`referenceSolution`, `emptySolution`, and every `Answers` val you declared — so a single
`testGraderGroup` call covers all your solutions at once.

## The built-in reference-solution check

Every problem automatically gets a test asserting that, on `referenceSolution`: the total is full
points, **no** partial (`partial = true`) rule fires, **every** full positive rule fires (or is
`notApplicable` because a higher rule fired), and **no** negative (penalty) rule fires. So you rarely
need to assert the reference case by hand.

## `testGrader` (isolation) vs `testGraderGroup` (joint): what to assert

`testGrader` runs one grader **in isolation** (ignoring `unless`); `testGraderGroup` grades the
assessment **jointly** (applying `unless`) and checks ≤ 1 of the group fires. They cover different
things, so use each for what it covers and don't duplicate.

For a solution `S`, assert with **one** `testGrader(X, S, fires)` the rule `X` whose *own case* `S`
is. You do **not** need `testGrader(Y, S, doesntFire)` for a sibling `Y` that is meant to be excluded
only by `unless`: such a `Y` may legitimately fire *in isolation* (graders need not self-guard against
higher rules — that is what `unless` is for), and the joint `testGraderGroup` is what guarantees `Y`
is suppressed so only `X` is awarded. Writing `testGrader(Y, S, doesntFire)` in that situation is not
just redundant, it can be **wrong** — it would fail on a correct grader that relies on `unless`.

Do keep a `testGrader(Y, S, doesntFire)` when `Y` genuinely should **not** fire on `S` **on its own
merits** — i.e. `S` is not `Y`'s case at all (a wrong / empty / malformed answer that `Y` must reject),
independently of any `unless`. Those pin the grader's own logic and the group cannot replace them (it
only bounds the count). Likewise keep `testGrader`s that nail down a `firesPartially(p)` boundary.
If a `testGraderGroup` and a fire-test for another grader together already imply that `Y` doesn't fire,
no need to test that `Y` doesn't fire.


The reference-solution check already covers `referenceSolution` (the full rule fires, no partial rule
fires), so you rarely need to assert that case by hand.

# Instructions for AI (Claude)

When developing a grader, do not edit the problem file, only the graders.
The only exceptions are: to add the import of the grader function and plug its name into the
`grader=...` argument of `grading(...)`; to set that rule's `partial` / `negative` / `unless` flags
(these are rule-level config, not grader logic); and to edit CLAUDE.md (see below).
If in doubt about the interpretation of the human readable grading rule, ask.
Be very precise in your thinking, always rethink whether your approach to the grader is correcting the problem correctly.
As the first step, write a docstring for the grader that describes (human readable) the algorithm to decide the grading rule.
Before continuing, ask the user whether the plan in that docstring is ok.
Inside the grader, make sure to add comments.
Develop defensively: in unexpected situations, the grader code should rather throw an exception than answer incorrectly.
Exceptions will be noticed during the grading process, and the grader can still be adapted; 
incorrect answers may grade students incorrectly.

Grading via testcases is ok. (E.g., to check whether an algebraic expression in x equals the reference solution, check equality for different x.)
As long as it is unlikely that this corrects incorrectly.
However, the grader must be deterministic. (No use of random generator in the grader code.
If "random" testcases are needed, they should be chosen randomly and then hardcoded in the grader code.)

Before writing a grader, read the problem as a whole; and understand also the explanations of the reference solution.
You can also read information about the lecture (e.g., online lecture script) if this is needed for 
correctly interpreting the grader. You can ask for URLs etc.

Collect information that is relevant for later AI runs in CLAUDE.md in the exam directory.
(E.g., information or guidance received. from the user)
