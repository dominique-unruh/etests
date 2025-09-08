# How to handle STACK specific problems?

## Question tags

* `moodleQuestionVariables` -- field of the same name in Moodle UI

## Answer element tags

E.g. `val answer = input(..., Tags(tags))`. Here `tags` can be the following STACK specific things:

* `moodleAllowWords` -- allowed words (even single and double letter words need to be enabled explicitly)
* `moodleInputType` -- algebraic inputs, matrices, etc.
* `moodleReferenceSolution` -- if STACK should be told a different reference solution (mostly because for input type matrix, STACK uses the reference solution to determine the matrix size)
* `moodleExtraOptions` -- extra options (called that way in the Moodle UI), e.g., `simp`
* `moodleInsertStars` -- how to treat implicit multiplication (like `ab` instead of `a*b`)