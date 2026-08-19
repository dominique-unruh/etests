# Exporting exams

There are three ways content leaves `etests`:

- **To Dynexite** — push each question into the RWTH exam platform (the live exam students sit).
- **Archival** — write a static, self-contained snapshot (YAML + HTML + PDF) next to the sources.
- **Printouts** — render the whole exam to a single PDF (e.g. a paper backup / reading copy).

All three need **Docker**: STACK parsing (Dynexite XML), and the `docker/html-to-pdf` Chromium +
puppeteer + MathJax converter (`Utils.htmlToPdf`) for PDFs.

## To Dynexite

Each problem corresponds to a Dynexite **item** that must already exist. Its id (the `<id>` in a
`…/items/<id>/edit` URL) goes in the problem's `dynexiteQuestionId` tag. The item's content is
generated as **Moodle/STACK question XML** (`MoodleStack.assessmentToQuestion` →
`MoodleStack.Quiz(...).prettyXml`) and uploaded over Dynexite's builder WebSocket, overwriting the
item's single block (`questionXML`, `title`, reachable points).

Trigger it by running a problem or exam **object's `main`** with the run option `uploadDynexite`:

- Set `run.option.for.problem = uploadDynexite` in `java.properties` (the option selects what running
  an `Assessment`/`Exam` object does; values: `runTests`, `extractStack`, `uploadDynexite`).
- Then run the object: in IntelliJ via the object's run config (Ctrl-Shift-F10), or
  `sbt "project exams; runMain <package>.<ProblemOrExam>"`.

What happens (`MarkdownAssessment.uploadToDynexite` → `Dynexite.markReviewedAndUpload`):

1. Runs the problem's self-tests first (a failure is reported but does not abort the upload).
2. Marks the item **reviewed** (the approval step) — this also serves as the interactive auth check.
3. Uploads the generated XML.
4. **Safety check:** the item's Dynexite name must equal `dynexiteQuestionName` (default = the
   problem's `name`); a mismatch throws and nothing is uploaded (guards against a stale/wrong
   `dynexiteQuestionId`). The id format is validated up front too.

Running the **`Exam`** object with `uploadDynexite` uploads every problem in turn, then prints the
exam URL (needs the `dynexiteCourseId` / `dynexiteExamId` tags on the exam).

**Authentication.** Uploading needs the teacher session cookie `dyn-orbit-teacher`. Grab its value
from the browser devtools after logging into Dynexite. It is stored in
`Utils.tempDir/dynexite-authcookie` (a user-private file; see `Utils.readOrPromptUserSecret`), prompted
for if absent, and automatically discarded + re-prompted on an HTTP 401 (expired cookie).

**Manual alternative (`extractStack`).** With `run.option.for.problem = extractStack`, running a
problem prints its Moodle/STACK XML and copies it to the clipboard (you have 60 s to paste). Useful for
pasting into Moodle/STACK, or into the Dynexite editor by hand. No upload and no auth cookie needed.

## Archival

The `ArchiveExam` task (`tasks/ArchiveExam.scala`) writes a static snapshot into an `archive/`
subdirectory **next to the exam source file** (e.g. `exams/y2025_pqc1/archive/`):

- `<ExamClass>.yaml` — the manifest.
- `<ProblemClass>.html` + `.pdf` — one pair per problem.
- `exam.html` + `exam.pdf` — all problems concatenated into one document.

Run it with:

```
sbt "project tasks; runMain ArchiveExam"
```

The exam to archive is the `current.exam` option (fully-qualified `Exam` class; prompted if unset).
Each problem is rendered **statically with the reference solution filled in**
(`RenderContext(dynamic := false, studentAnswers := referenceSolution)`), so the snapshot doubles as a
solution key. PDFs come from the HTML via `Utils.htmlToPdf`.

Full directory layout and manifest schema: see [`archive.md`](archive.md).

## Printouts

For a single PDF of the **whole exam** (the kind used as `exam-printout.pdf` for a paper copy), there
are two paths, differing in whether solutions are shown:

- **Blank question sheet** — `Exam.renderExam(outputFile: Path, showSolutions: Boolean = true)`. Renders
  every problem statically with **no answers**, concatenates them, and writes a PDF via
  `Utils.htmlToPdfAsync`. Passing `showSolutions = false` makes every `SolutionElement` (explanations,
  grading rules, graders) render as nothing, giving the students'-view printout. Run the bundled task:

  ```
  sbt "project tasks; runMain RenderBlankExam"
  ```

  (`RenderBlankExam` renders `current.exam` to `exam-printout.pdf` next to the exam sources.)

  Whether solutions appear in static output is controlled solely by `RenderContext.showSolutions`
  (default `true`; see `SolutionElement.renderHtml`). In dynamic rendering the `<etest-solution>` web
  component is always emitted and its visibility is a client-side toggle.

- **Solution snapshot** — the `exam.pdf` produced by [Archival](#archival) above. Same concatenated
  layout but rendered with the reference solution filled in. Take
  `<exam>/archive/exam.pdf` and rename/print it.

Both go through the `docker/html-to-pdf` Chromium/MathJax converter, so math (`\(...\)` / `$...$`)
typesets in the output.
