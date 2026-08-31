package externalsystems

import assessments.Exam.{courseName, ects, semester}
import assessments.{Exam, InterpolatedString, RegistrationNumber}
import assessments.InterpolatedString.iraw
import utils.{IndentedInterpolator, LaTeX}
import utils.LaTeX.escape

import java.nio.file.{Files, Path}
import java.time.LocalDate
import java.time.format.DateTimeFormatter


object Schein {
  val responsiblePerson: String = "Prof. Dr. Dominique Unruh"
  case class Student(name: String,
                     registrationNumber: RegistrationNumber,
                     email: Option[String] = None,
                     grade: Option[Double]) {
    def gradeString = f"${grade.get}%.1f"
    def withGrade(grade: Double): Student = copy(grade = Some(grade))
  }

  private val logo = Path.of("/home/unruh/cloud/qis/shared/agnes-uvalic/templates-and-design/institute-logo/02_bildmarke_und_text_EN/pdf/rwth_lehrstuhl_quanteninformationssysteme_en_rgb.pdf")

  def pdf(exam: Exam, student: Student): Array[Byte] = {
    val source = latexSource(exam, student)
    val logoBytes = Files.readAllBytes(logo)
    LaTeX.latexToPDF(source, Map("logo.pdf" -> logoBytes))
  }

  def latexSource(exam: Exam, student: Student): String = {
    val date = LocalDate.now().format(DateTimeFormatter.ofPattern("MMMM d, yyyy"))
    indraw"""
\documentclass[12pt]{article}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage{geometry}
\usepackage{graphicx}

\parindent=0pt
\pagestyle{empty}

\begin{document}

\begin{flushright}
\includegraphics[width=.5\linewidth]{logo.pdf}
\end{flushright}

\hrule

\bigskip

\begin{center}
\Large Certificate
\end{center}


\textbf{${escape(student.name)}} (student registration number ${escape(student.registrationNumber.number)})

\bigskip

successfully attended the course


\begin{center}
\textbf{${escape(exam.tags(courseName))}}
\end{center}


(${exam.tags(ects)} ECTS credits) during ${escape(exam.tags(semester).season.lowerCase)} semester ${exam.tags(semester).year}.

\bigskip

Final grade: ${escape(student.gradeString)}

\bigskip

Aachen, ${escape(date)},

\begin{flushright}
$$\overline{\textrm{\quad(${escape(responsiblePerson)})\quad}}$$
\end{flushright}

\end{document}
        """
  }

}