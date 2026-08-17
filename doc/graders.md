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
- The outcome type:
  - Is the rule satisfied positively (makes sense for rules giving points): `correct`.
  - If the rule determines that an answer is not fully correct but still gives full points: `partiallyCorrectFullPoint`
  - If the rule determined the answer is not fully correct and partial points are given: `partiallyCorrect`
  - If the answer is incorrect, zero points (or point substraction, depending on the kind of rule): `wrong`
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
  (`Outcome`: `unspecified` (default), `missing`, `inapplicable`, `correct`, `incorrect`,
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

# Instructions for AI (Claude)

When developing a grader, do not edit the problem file, only the graders.
The only exception is to add the import of the grader function, and to plug in the name of the grader function in the `grader=...` argument to `grading(...)`.
And to edit CLAUDE.md (see below).
If in doubt about the interpretation of the human readable grading rule, ask.
Be very precise in your thinking, always rethink whether your approach to the grader is correcting the problem correctly.
As the first step, write a docstring for the grader that describes (human readable) the algorithm to decide the grading rule.
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
