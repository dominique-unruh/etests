package exam.example

import assessments.Exam
import assessments.Exam.{courseName, examDate}
import utils.Tag.Tags

import java.time.LocalDate

object ExampleExam extends Exam(
  name = "example exam",
  tags = Tags(
    examDate := LocalDate.parse("2025-09-30"),
    courseName := "Example course")
)(
  ExampleProblem,
  ExampleProblem2
)
