# How to write graders?

Graders are written **inline** in grading elements. Each `grading(text, grader)` (see
[questions.md](questions.md)) is a page element carrying a rule text plus the grader block
that awards its points; interpolate it into the `question` markdown to show its live grading
box. A problem usually has several, one per rule, and their points are summed.

The grader block itself is a **grade block** (see below): it runs with `max = 0` local
points, and you finish it with `done()` (there is no `return`; `abort()` is not allowed in a
top-level grading element). The old single `grade()` method still exists but is
`@deprecated` in favour of inline graders.

## Writing rules

### Misc

* Do `import assessment.GradingContext.*` to get all the features below
* `points += n` adds points
* `comments += "hello"` adds a comment `hello` (interpreted as markdown)
* `answers(element) = ...` can **change** an answer given by the student
  (useful for doing some cleanup like trimming whitespaces or replacing special cases)
* `outcome = Outcome.correct` sets a verdict flag independent of the points
  (`Outcome`: `unspecified` (default), `missing`, `inapplicable`, `correct`, `incorrect`,
  `partiallyCorrect`, `partiallyCorrectFullPoints`). Shown as a colored badge in the webapp
  grading view. When set
  inside a nested grade block, the last non-`unspecified` value propagates to the enclosing grader.

### Try using grade blocks for individual questions

TODO: Explain

Advantages:
* comments, points inside grade block can be undone (`abort()`)
* comments are presented hierarchically to student
* changes to `answers(element)` are local to a grade block
* points are always set to 0 inside a grade block. The points you give will be added outside the grade block
* use `done(...)` to finish the grade block (like a return-statement) when that subproblem
  is finished (avoids possibly very nested if-then-else statements)
* `combinatorialGrader` is even more powerful (but has all of the above, too)

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