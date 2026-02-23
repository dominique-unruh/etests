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

  // The actual question text as Markdown
  lazy val question = md"""..."""

  // Explanation of the solution as Markdown (shown to students after exam)
  lazy val explanation = md"""..."""

  // Grading rules as Markdown (shown to students after exam)
  lazy val gradingRules: InterpolatedMarkdown[Element] = md"""..."""

  // Additional configuration options (optional)
  override val tags = Tags(tagname := content, tagname2 := content2)  
  
  // Function that computes the actual grade
  def grade()(using context: GradingContext, exceptionContext: ExceptionContext): Unit = {
    throw NoGraderYetException
  }
```

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

TODO

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

