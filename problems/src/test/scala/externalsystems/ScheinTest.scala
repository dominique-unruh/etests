package externalsystems

import externalsystems.Schein.Semester.Winter
import externalsystems.Schein.{Course, Student}
import utils.{LaTeXException, LaTeXTest}

object ScheinTest {
  def main(args: Array[String]): Unit = {
    val course = Course(name="My nice course", semester = Winter, year = 2025, ects = 6)
    val student = Student(name="Otto Waalkes", registrationNumber = "0123456", grade=2)
    val latex = Schein.latexSource(course, student)
    println(latex)
    try {
      val pdf = Schein.pdf(course, student)
      LaTeXTest.showPDF(pdf)
    } catch {
      case e: LaTeXException =>
        println(e.fileString("latex.log"))
        throw e
    }
  }
}
