# Java.properties

The file `java.properties` in the root of this project
contains various configuration options.

Format: `option.name = value` in each line.

Existing options:

* [REMOVED] `dynexite.results.json.XXX = path` Where is the Dynexite JSON export for exam XXX stored? (As downloaded from Dynexite.) XXX is the short name of the Exam class (e.g. Iqc1Exam).
* [REMOVED] `dynexite.results.pdfs.XXX = path` Where is the ZIP with the Dynexite result PDFs stored? (As downloaded from Dynexite.) XXX is the short name of the Exam class (e.g. Iqc1Exam).
* `student.report.dir = path` TODO: document
* `current.exam = CLASS` The "current" exam. Some tools in this project operate on one exam, they use this option to decide which. CLASS is the long name of the Exam class (e.g., `exam.example.ExampleExam`).
* `sciebo.username = username` For automated Sciebo uploads.
* `sciebo.password = password` For automated Sciebo uploads.
* `run.option.for.problem = OPTION` Can be `extractStack` or `runTests`. Decides what to do when an individual problem is executed (Ctrl-Shift-F10 in IDEA).
* `grading.timeout = 60s` Timeout when grading (grader fails then, better than just waiting forever)
* `cache.file = path` The tool needs a cache file to avoid rerunning costly operations all the time.
  Here you can give the path which file to use (e.g. `/tmp/cache.data`). 
  The file will be created by the tool.
