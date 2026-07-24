# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`etests` is a tool for authoring, previewing, and grading university exam questions (primarily
cryptography courses at RWTH Aachen). Each exam question is a Scala object that produces Markdown +
interactive page elements and a grading function. Questions integrate with external systems
(Dynexite, Moodle/STACK, RWTHonline, Sciebo/Nextcloud) and can be rendered to HTML/PDF or graded in
bulk. There is a Play web app for live preview during authoring.

## Build & run

SBT multi-project build (Scala 3.6.4, sbt 1.10.1). `fork := true`.

- **Run the preview web app:** `sbt "project webapp; run"` (serves on http://localhost:9000/preview/).
  `make show` does the same and opens a browser. IntelliJ run config `webapp` exists.
- **Compile:** `sbt compile` (or `sbt "project <name>; compile"`).
- **All tests:** `sbt test`. Per-project: `sbt "project problems; test"`.
- **Single test:** `sbt "project problems; testOnly assessments.stack.StackParserTest"`.
  Single test method: `sbt "testOnly *StackParserTest -- -z \"substring of test name\""` (ScalaTest `-z`).
- **Bulk grading / archiving:** run a `Task` object from the `tasks` project, e.g.
  `sbt "project tasks; runMain TaskGradeEveryone"` (also `ArchiveExam`, `TaskGradesToRWTHOnline`).
  `ArchiveExam` writes a static exam snapshot (YAML manifest + per-problem HTML/PDF); format in
  `doc/archive.md`.

TypeScript (`webapp/app/assets/js/*.ts`) and SCSS are compiled by sbt-web plugins during
`webapp/run`; no separate npm build step.

## Modules (see `build.sbt`)

- **`problems/`** — the engine. Assessment/Exam model, page elements, Markdown/HTML interpolation,
  STACK+Sympy math, Docker helpers, external-system integrations. Everything else depends on it.
- **`exams/`** — the actual exam question sources. **Separate git repo, gitignored at repo root**
  (`/exams`). The `exams/` dir itself is the source root: one flat folder per exam whose name
  equals its single-segment package (e.g. `y2025_pqc1/` = `package y2025_pqc1`), with resources
  (png/svg) sitting next to the sources. `exams/exams.yaml` has an `archived:` list of folders
  excluded from compilation (old exams kept only for their `archive/` snapshots, and broken/WIP
  exams) but still resource-included. `test/` holds the test sources (`Test` scope). Depends on
  `problems`. Exam discovery only finds `Exam` objects in single-level packages (see `Exam.exams`).
- **`webapp/`** — Play Framework preview server. Depends on `problems` and `exams`.
- **`tasks/`** — runnable `Task` objects (grade-everyone, export, push grades). Depends on `exams`.
  Source root is the project dir itself.

## Core architecture

**Question authoring model** (`problems/src/main/scala/assessments/`):
- A question is an `object X extends MarkdownAssessment` with `name`, `reachablePoints`, `question`
  (and optional `explanation`, `gradingRules`) as `md"""..."""` interpolated Markdown, plus a
  `grade()(using GradingContext, ExceptionContext)` method. Boilerplate documented in
  `doc/questions.md`; working example: `problems/src/main/scala/example_exam/ExampleProblem.scala`.
- An `Exam` is an `object extends Exam(name, tags)(problem1, problem2, ...)`. Exams and their
  problems are discovered by classpath scanning (`io.github.classgraph.ClassGraph`).
- `MarkdownAssessment` lazily builds an `Assessment` (the lower-level model) by reflecting over its
  own vals to bind page-element names. `$foo` in the Markdown interpolates the `val foo` page
  element into the rendered page.

**Page elements** (`assessments/pageelements/`): `InputElement`, `MultipleChoice`, `ImageElement`,
`MathElement`, `MathPreviewElement`, etc. Input fields default to STACK-formula inputs. Rendering is
driven by a `RenderContext` (dynamic vs. static, whose answers to show).

**Interpolation layer**: `InterpolatedMarkdown`/`InterpolatedHtml`/`InterpolatedString` +
`IndentedInterpolator` implement the `md"..."` / `ind"..."` string interpolators that mix Scala
values (page elements, HTML) into templates. `ElementName` identifies interpolated slots.

**Grading**: `Grader` / `GradingContext` (`import assessments.GradingContext.*`). Inside `grade()`,
`points += n`, `comments += "..."`, `answers(element) = ...`. Grade blocks and `combinatorialGrader`
give undoable/hierarchical scoring. Graders run under a timeout (`grading.timeout`). See
`doc/graders.md` — **important:** exception *handlers* (`try/catch`) are forbidden in graders (they
hide grading mistakes); use safe helper functions that return `Option` instead. Throwing is allowed
and denotes an unimplemented case.

**Math** (`doc/math.md`, `doc/stack.md`): two representations. `StackMath` = terms in Scala (name is
historical; unrelated to STACK), best for programmatic manipulation. `SympyExpr` = Python/sympy
objects via ScalaPy, for built-in symbolic math. Convert with `math.toSympyMC(...)`; a
`given MathContext` in scope supplies default variable values / test values used by comparison ops
(`algebraicEqual`, `checkEqualityNew`, `enumerateMapped`, ...).

**Docker-backed services** (`utils/Docker.scala`, `docker/`): several operations shell out to Docker
images built on demand from the `docker/` subdirs.
- `docker/stack-parser` — parses STACK/Maxima expressions (used by `StackParser`).
- `docker/html-to-pdf` — Chromium/puppeteer HTML→PDF with MathJax (used when exporting).
- LaTeX/TikZ/quantikz rendering (`utils/LaTeX.scala`) also runs in Docker.
Docker must be available for parsing, PDF export, and LaTeX-image questions.

**External systems** (`externalsystems/`): `Dynexite` (RWTH exam platform import/export),
`MoodleStack`, `RWTHOnlineGrades`, `Schein`, `Sciebo`/Nextcloud upload, `Spreadsheet` (CSV/ODF).

**Web app**: routes in `webapp/conf/routes` → `AssessmentController` (preview pages, load/save
answers, feedback, Dynexite links/PDFs). Config in `webapp/conf/application.conf`; startup modules
`SystemPropertiesModule`, `CacheInvalidationModule`.

## Configuration: `java.properties`

Root-level `java.properties` (gitignored; contains credentials) holds runtime options read via
`Utils.loadSystemProperties()`. Format: `option.name = value` per line. Key options (full list in
`doc/java-properties.md`): `current.exam` (fully-qualified Exam class the single-exam tools operate
on), `grading.timeout`, `cache.file` (path for the persistent cache — created on first run),
`sciebo.username`/`sciebo.password`, `run.option.for.problem` (`extractStack` or `runTests`; chosen
when running an individual problem object from the IDE).

## Notes

- Backslashes in Markdown math (`\(x+y\)`) are correct and must NOT be escaped, even though IntelliJ
  flags them as errors.
- Question `val` names surface in Dynexite JSON exports — keep them informative.
- The `exams/` working tree is a distinct repository; committing question changes there is separate
  from committing engine changes in this repo.
