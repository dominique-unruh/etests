# How to write questions?

See also `ExampleProblem.scala` as a working example.

## Overall boilerplate

Each exam question is a single Scala file, following the following boilerplate.

Before the exam correction, only `question` is required to be properly filled in.

```scala
object ProblemName extends MarkdownAssessment {
  // Must match the class name up to spaces, punctuation
  override val name = "Problem Name"
  override val reachablePoints: Points = 10000000

  // The actual question text as Markdown.
  // Explanation and grading elements are page elements interpolated into it
  // with `$` (they are only shown in the solution view, see below).
  lazy val question = md"""...
    $explanation
    $gradingRule
  """

  // Explanation of the solution (shown to students after exam)
  lazy val explanation = explain(md"""...""")

  // A grading rule: its Markdown text is the rule shown to students, and the
  // trailing `{ ... }` block is the grader that awards the points for that rule.
  lazy val gradingRule = grading(md"""...""") {
    if (answer.stringValue.trim == "10")
      points += reachablePoints
      done()
  }

  // Additional configuration options (optional)
  override val tags = Tags(tagname := content, tagname2 := content2)  
}
```

Note: unlike the question text, the explanation and grading elements are only
shown in the **solution** view (after the exam / while authoring), not to students during
the exam. They must be placed in the `question` markdown with `$` where you want them to
appear; there is no longer a fixed layout that renders them in separate boxes.

Grading is done by **inline graders**: each `grading(...) { ... }` element carries both a
rule text and the grader block that scores it. Points are summed across all grading
elements. (The old single `grade()` method still exists but is `@deprecated`.)

## Question text

The question text (in `val question = ...`) can be arbitrary Markdown.
This Markdown can additionally contain `$`-sequences for Scala-code that includes page elements.
Page elements can be, e.g., input-fields, previews, images...

Note: You can write `\(formula\)` for math (interpreted with MathJax).
Note that `\` does not need to be quoted but sadly Intelli/J IDEA thinks it needs to be quoted,
so it will highlight all `\` as errors.

Example: In the question text, write `look at this: $picture` and in the Scala code define:
```
lazy val image = ImageElement.fromSVGResource("elitzur_vaidman.svg", getClass)
```

The various possible page elements are described now:

### Images

Images can be included by creating an ImageElement. Easiest is to use some of the helper functions below.
In all cases, you can then just use `$image` in the question text to include the image.

```scala
lazy val image = ImageElement.fromSVGResource("image.svg", getClass)
```
This creates the image from an SVG file.
The file `image.svg` must be in the same directory as the current question source file. 

```scala
lazy val image = latex("""\(x+y\)""")
```
Renders LaTeX code as a picture.
Note: for formulas as in the example above, this is not the best idea because you can
simply write `\(x+y\)` in the markdown.

```scala
lazy val image = tikz("""...""")
lazy val image = quantikz("""...""")
```
Renders LaTeX code as a picture. The code `...` will be set inside a `tikz` or `quantikz` environment.
This allows to draw pictures or quantum circuits.


### Input fields

To define an input field, the most common syntax is:

```scala
val answer = input("reference solution")
val answer = input("reference solution", Tags(tagname := content, tagname2 := content2, ...))
```
The second variant allows to configure addition options, e.g., for [Moodle/STACK specific options](stack.md)

Note: The name of the question variable used here (`answer`) 
should be informative since it shows up in various places (e.g., Dynexite JSON files...).

Note: By default, `input` produces a Stack-formula-input.

### Previews

A preview can be added by including `${preview(answer)}` in the question text Markdown.
Where `answer` is the variable containing the input element. (See above.) 
Since one never refers to the preview from the remaining code, it is not necessary to assign it to a `val` first.

### Explanations and grading rules (solution elements)

Explanations and grading rules are **solution elements** (`SolutionElement`): page elements
that are only rendered in the solution view, not shown to students during the exam. They are
created with helper functions and then interpolated into the `question` markdown with `$`
wherever you want them displayed:

```scala
lazy val explanation = explain(md"""...""")   // styled as an explanation box

lazy val gradingRule = grading(md"""...""") {  // styled as a grading-rules box
  ... grader block, see below ...
}
```

`explain(...)` produces an `ExplanationElement` (styling `explanation`); `grading(...)`
produces a `GradingElement` (styling `grading`), which selects the CSS class
`solution-explanation` / `solution-grading`. Put `$explanation`, `$gradingRule` into the
`question` markdown to place them.

A `grading(text) { grader }` element carries both the rule text *and* the grader that awards
its points (see [graders.md](graders.md)). A problem typically has several, one per rule.
During feedback each element runs its grader (under `grading.timeout`), and the points
reached across all grading elements are summed and displayed by the `etest-points-reached`
component in the sidebar.

