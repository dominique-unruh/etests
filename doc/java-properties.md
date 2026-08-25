# Java.properties

The file `java.properties` in the root of this project
contains various configuration options.

Format: `option.name = value` in each line.

Existing options:

* [REMOVED] `dynexite.results.json.XXX = path` Where is the Dynexite JSON export for exam XXX stored? (As downloaded from Dynexite.) XXX is the short name of the Exam class (e.g. Iqc1Exam).
* [REMOVED] `dynexite.results.pdfs.XXX = path` Where is the ZIP with the Dynexite result PDFs stored? (As downloaded from Dynexite.) XXX is the short name of the Exam class (e.g. Iqc1Exam).
* `student.report.dir = path` TODO: document
* `current.exam = CLASS` The "current" exam. Some tools in this project operate on one exam, they use this option to decide which. CLASS is the long name of the Exam class (e.g., `example_exam.ExampleExam`).
* `sciebo.username = username` For automated Sciebo uploads.
* `sciebo.password = password` For automated Sciebo uploads.
* `run.option.for.problem = OPTION` Can be `runTests`, `extractStack`, or `uploadDynexite`. Decides what to do when an individual problem (or exam) object is executed (Ctrl-Shift-F10 in IDEA). See `doc/exporting.md`.
* `grading.timeout = 60s` Timeout when grading (grader fails then, better than just waiting forever)
* `stackparser.url = http://localhost:8080` Base URL of the stack-parser HTTP service used by `StackParser`. If set, expressions are parsed by POSTing to `<url>/parse` (a warm daemon; see `services/stack-parser/etests-service`). If unset, a one-shot Docker container (`docker/stack-parser`) is spun up per parse instead.
