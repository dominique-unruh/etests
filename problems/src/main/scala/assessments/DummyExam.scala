package assessments

/** A placeholder [[Exam]] with no problems and no grading exceptions. Used where an [[Exam]] is
 * required by a signature but none is meaningfully available — e.g. the grader test helpers (which
 * grade a standalone problem) and Moodle/Dynexite export (which never invokes a grader). */
object DummyExam extends Exam("Dummy exam")()
