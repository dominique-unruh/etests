# How to write grading rules?

Grading rules are written inline in the question text.
A single grading rule is a basic instruction for grading.
E.g., "the number above is correct up to a constant factor: 3 points".
A grading rule is added by writing
`${grading(text=md"""TEXT"""), points=POINTS, grader=GRADER, name="NAME"}`
inside the question markdown.
Here:
- `TEXT` is a human readable description of the grading rule (in markdown format).
- `POINTS` is the number of points reachable if this grading rule triggers positively.
- `GRADER` is the function that computes, given the student's answers, whether the grading rule triggers. (And how many points are given or deducted.)
- `NAME` is the name of the grading rule. Must be a valid identifier.

A problem can have many grading rules.
The grading rules are not shown to the student during the exam; but TEXT is shown after the exam.  

The position of the grading rule does not matter for the underlying grading logic,
however the position matters for the interpretation of the TEXT
(because it usually refers to the inputs above it).

# How to write graders?

For readability, only elementary graders should be directly included in the `grading(...)` call.
Elementary graders include:
- `missingGrader`: To indicate that a grader is still to be added.
- `equalsReference`: Gives full points if the student solution for a given input field matches the reference, 0 otherwise.

Otherwise each grader should be placed in files in the subpackages `graders` (inside the package where the problem is).
Grader functions for problem object `X` should be in the object `graders.XGrader` and imported in the problem's file.
Helper functions can be placed in the same file, or, if shared between problems, in another file in the `graders` package.
Helper functions must not be shared between different exams!

A grader should determine the following:
- The number of points added (or removed).
- The outcome type. **`correct` means the answer to the task is fully correct** — not merely that
  this rule fired. A partial-credit rule that fires positively (e.g. "correct up to sign", "correct
  but not normalized", "any quantum state") does **not** return `correct`; the answer is not fully
  correct, so it returns `partiallyCorrect` (or `partiallyCorrectFullPoints`, see below).
  - The answer is fully correct: `correct`. Typically only the top-priority rule that captures the
    fully-correct answer and awards full points.
  - The answer is not fully correct but this rule gives full points anyway: `partiallyCorrectFullPoints`.
    E.g. a follow-up mistake that we choose not to penalize a second time (the sub-answer is wrong,
    but full points are awarded because it is consistent with an earlier, already-penalized mistake).
  - The answer is not fully correct and partial points are given: `partiallyCorrect`.
  - If the answer is incorrect, zero points (or point subtraction, depending on the kind of rule): `incorrect`
  - If the rule is not applicable: `notApplicable`. E.g., if there's a rule "correct answer: 3 points",
    followed by a rule "correct up to sign: 2 points", then the second rule would return `notApplicable` if the first
    one triggers.
  - If no points are given because an answer is missing: `missing`
  - The outcome `unspecified` is not allowed. 

A grader can additionally produce a number of comments.
These are shown to the students together with the points.
For simple cases, they are not necessary.

## Writing rules

### Misc

* Do `import assessment.GradingContext.*` to get all the features below
* `points += n` adds points
* `comments += "hello"` adds a comment `hello` (interpreted as markdown)
* `answers(element) = ...` can **change** an answer given by the student
  (useful for doing some cleanup like trimming whitespaces or replacing special cases)
* `outcome = Outcome.correct` sets a verdict flag independent of the points
  (`Outcome`: `unspecified` (default), `missing`, `notApplicable`, `correct`, `incorrect`,
  `partiallyCorrect`, `partiallyCorrectFullPoints`). Shown as a colored badge in the webapp
  grading view. When set
  inside a nested grade block, the last non-`unspecified` value propagates to the enclosing grader.

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
  if (checkSomeMathProperty(parse(string))) points += 3
catch
  case _ : SyntaxError =>
...
```
then you need to define, e.g., a helper function 
`def safeParse(string) = try Some(parse(string)) catch e: SyntaxError => None`
or similar (better: in a way that produces feedback that can be given to the student).
And then in the grader itself you do
```scala 3
val parsed = safeParse(string)
...
if (parsed != None && checkSomeMathProperty(parsed.get)) points += 3
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
test solution by the chain/overall tests below (via reflection), alongside these two predefined ones.

## `testGrader` — a single grading rule

```scala 3
testGrader(grader, solution, outcome = …, points = …)
```

Asserts that grading `solution` with the grading rule named `grader` yields the given `outcome`
(an `Outcome`: `correct`, `partiallyCorrect`, `partiallyCorrectFullPoints`, `incorrect`,
`notApplicable`, `missing`) and/or the given `points`. Either assertion is skipped if its argument is
omitted. `grader` is the rule name (the `name="…"` you passed to `grading(...)`) or the grading element
itself. Example:

```scala 3
testGrader("correctGrader", referenceSolution, outcome = correct, points = 10)
testGrader("grader12", solHalf, outcome = partiallyCorrect, points = 5)
testGrader("grader12", referenceSolution, outcome = notApplicable, points = Points.zero) // higher rule won
testGrader("grader12", emptySolution, outcome = missing, points = Points.zero)
```

Related: `testGraderThrows(grader, solution)` asserts that the grader *throws* on that solution (graders
throw to flag a case they deliberately do not handle — see above).

## `testGraderChain` — priority / mutual exclusion

```scala 3
testGraderChain(name = "distinctGraders",
  graders = Seq("correctGrader", "grader12", "grader1")) // descending priority order
```

Given the graders of one input (or one answer group) in **descending priority order**, this checks the
whole chain behaves consistently for a solution. A grader *triggers* if its outcome is `correct`,
`partiallyCorrect`, or `partiallyCorrectFullPoints`, or it awards nonzero points. The check asserts:
- once some grader triggers, **every later** (lower-priority) grader is `notApplicable`; and
- any grader that is `notApplicable` has **some earlier** (higher-priority) grader that triggered.

If `solution` is omitted (the default), the chain check runs **once for each** test solution —
`referenceSolution`, `emptySolution`, and every `Answers` val you declared — so a single
`testGraderChain` call covers all your solutions at once.

## You do not need `testGrader` for every grader × solution

The chain test already pins down which graders must *not* fire. So for a given solution it is enough to
assert, with **one** `testGrader`, the grader that is *supposed* to fire (its positive `outcome` and
`points`). You do **not** need to add `testGrader(…, notApplicable)` / `testGrader(…, incorrect)` for
the other graders on that same solution: if graders `X, Y, Z` form a tested chain and you assert
`testGrader("Y", sol, outcome = correct)`, the `testGraderChain` over `X, Y, Z` guarantees that `X`
does not fire (a higher-priority `X` firing would make `Y` `notApplicable`, contradicting `correct`)
and that `Z` is `notApplicable` (a later grader after a trigger). So one positive `testGrader` per
solution plus the chain gives full coverage; add extra `testGrader` assertions only where you want to
nail down a specific `outcome` the chain does not determine (e.g. distinguishing `incorrect` from
`missing`, or a `partiallyCorrectFullPoints` case).

# Instructions for AI (Claude)

When developing a grader, do not edit the problem file, only the graders.
The only exception is to add the import of the grader function, and to plug in the name of the grader function in the `grader=...` argument to `grading(...)`.
And to edit CLAUDE.md (see below).
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
