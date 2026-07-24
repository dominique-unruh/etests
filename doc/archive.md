# Exam archive format

The `ArchiveExam` task (`tasks/ArchiveExam.scala`) exports a static, self-contained snapshot of an
exam: one YAML manifest plus a rendered HTML+PDF per problem (and one combined HTML+PDF for the whole
exam). This is the human-readable record kept alongside each exam's sources.

## Producing an archive

```
sbt "project tasks; runMain ArchiveExam"
```

The exam to archive is the `current.exam` option (fully-qualified `Exam` class name); prompted via
popup if unset. See `doc/java-properties.md`.

Output goes into an `archive/` subdirectory **next to the exam source file**
(`exam.sourceFile.getParent/archive`), e.g. `exams/y2025_pqc1/archive/`. The directory
is created if missing; existing files with the same basename are overwritten.

## Directory contents

- `<ExamClass>.yaml` — the manifest (basename = simple class name, e.g. `Pqc1Exam.yaml`).
- `<ProblemClass>.html` + `<ProblemClass>.pdf` — one pair per problem. Basename = the problem
  object's simple class name (e.g. `ApplyTensor.html`, `ApplyTensor.pdf`).
- `exam.html` + `exam.pdf` — all problems concatenated (separated by `<hr/>`) into one document.
  (Older archives predate this and may contain only per-problem files.)

## Rendering

Each problem is rendered **statically** with the reference solution filled in:
`RenderContext(dynamic := false, studentAnswers := problem.referenceSolution)`. The page contains,
in order: the problem body (`<h1>` = problem name), an `Explanation` section, and a `Grading rules`
section. HTML uses `Assessment.htmlHeaderStatic` (includes MathJax config for `$...$` / `\(...\)`).
PDFs are generated from the HTML via `Utils.htmlToPdf` (the `docker/html-to-pdf` Chromium/MathJax
converter — Docker must be available).

## Manifest schema (`<ExamClass>.yaml`)

Top-level keys (produced by `ExamDescription`):

| key | meaning |
|-----|---------|
| `name` | full exam name (e.g. `Post-quantum Cryptography, WS 2025/26, 1st`) |
| `courseName` | from the exam's `courseName` tag |
| `examDate` | from the exam's `examDate` tag, `YYYY-MM-DD` |
| `reachablePoints` | total points across all problems |
| `exportedOn` | date the archive was generated, `YYYY-MM-DD` |
| `problems` | ordered list of problem entries (exam order) |
| `rendered` | exam-level rendered files (`exam.html`, `exam.pdf`) — absent in older archives |

Each `problems` entry (`ProblemDescription`):

| key | meaning |
|-----|---------|
| `name` | problem display name |
| `class` | fully-qualified problem object class (e.g. `exam.y2025.pqc1.ApplyTensor`) |
| `reachablePoints` | points for this problem |
| `rendered` | its rendered files: `[<basename>.html, <basename>.pdf]` |

Example:

```yaml
exportedOn: '2026-03-24'
examDate: '2026-03-03'
courseName: Post-quantum Cryptography
name: Post-quantum Cryptography, WS 2025/26, 1st
reachablePoints: 100
problems:
- name: Apply Tensor
  class: exam.y2025.pqc1.ApplyTensor
  reachablePoints: 5
  rendered:
  - ApplyTensor.html
  - ApplyTensor.pdf
```

Field order in the YAML is fixed by `@JsonPropertyOrder` on the case classes, not alphabetical.
