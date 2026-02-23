# How to handle STACK specific problems?

## Question tags

* `moodleQuestionVariables` -- field of the same name in Moodle UI

## Input element tags

E.g. `val answer = input(..., Tags(tags))`. Here `tags` can be the following STACK specific things:

* `moodleAllowWords` -- allowed words (even single and double letter words need to be enabled explicitly)
* `moodleInputType` -- algebraic inputs, matrices, etc.
* `moodleReferenceSolution` -- if STACK should be told a different reference solution (mostly because for input type matrix, STACK uses the reference solution to determine the matrix size)
* `moodleExtraOptions` -- extra options (called that way in the Moodle UI), e.g., `simp`
* `moodleInsertStars` -- how to treat implicit multiplication (like `ab` instead of `a*b`)

## Utility functions for working with STACK results in grader

In the following `input` is always an input element.

* `input.math` -> Parsed into our math representation (called `StackMath` currently but doesn't have anything to do with STACK).
  Warning: the same string gets parsed differently by different input elements because the STACK parser takes the
  input element options into account.
* `input.mathTry` -> Same as `.math` but returns `StackMath.noAnswer` instead of a syntax error, and informs the
  student of the syntax error via a comment. (Uses the variable name of `input` unless `input` has the tag `humanName`)
* `string.math(input)` -> Parsed into our math representation (`string` is parsed, using the parser of `input`)
* `input.refmath` -> Reference solution parsed into our math representation
* `input.stringValue` -> Just the value from the input field
* All of the above use the answer given by the student, unless we changed it using `answers(input) = ...` then the changed
  text is used!
* See also `math.md` for how to work with `StackMath` objects
