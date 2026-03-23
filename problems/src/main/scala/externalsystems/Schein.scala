package externalsystems

import assessments.InterpolatedString
import assessments.InterpolatedString.iraw
import utils.{IndentedInterpolator, LaTeX}
import utils.LaTeX.escape

import java.nio.file.{Files, Path}
import java.time.LocalDate
import java.time.format.DateTimeFormatter


object Schein {
  enum Semester {
    case Winter
    case Summer
    def lowerCase: String = toString.toLowerCase
  }
  case class Course(name: String,
                    semester: Semester,
                    year: Int,
                    ects: Int,
                    responsible: String = "Prof. Dr. Dominique Unruh")
  case class Student(name: String,
                     registrationNumber: String,
                     grade: Double) {
    def gradeString = f"$grade%.1f"
  }

  private val logo = Path.of("/home/unruh/cloud/qis/shared/agnes-uvalic/templates-and-design/institute-logo/02_bildmarke_und_text_EN/pdf/rwth_lehrstuhl_quanteninformationssysteme_en_rgb.pdf")

  def pdf(course: Course, student: Student): Array[Byte] = {
    val source = latexSource(course, student)
    val logoBytes = Files.readAllBytes(logo)
    LaTeX.latexToPDF(source, Map("logo.pdf" -> logoBytes))
  }

  def latexSource(course: Course, student: Student): String = {
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


\textbf{${escape(student.name)}} (student registration number ${escape(student.registrationNumber)})

\bigskip

successfully attended the course


\begin{center}
\textbf{${escape(course.name)}}
\end{center}


(${escape(course.ects.toString)} ECTS credits) during ${escape(course.semester.lowerCase)} semester ${escape(course.year.toString)}.

\bigskip

Final grade: ${escape(student.gradeString)}

\bigskip

Aachen, ${escape(date)},

\begin{flushright}
$$\overline{\textrm{\quad(${escape(course.responsible)})\quad}}$$
\end{flushright}

\end{document}
        """
  }

}