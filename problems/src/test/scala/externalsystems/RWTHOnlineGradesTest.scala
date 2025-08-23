package externalsystems

import externalsystems.RWTHOnlineGrades.Entry
import org.scalatest.funsuite.AnyFunSuiteLike

import java.nio.file.Path

class RWTHOnlineGradesTest extends AnyFunSuiteLike {
  test("read") {
    val path = Path.of("/home/unruh/cloud/qis/lectures/2025-intro-qc/rwth-exam-registrations.csv")
    val rwth = RWTHOnlineGrades.load(path)
    val row = rwth.byRegistrationNumber("***REMOVED***")
    println(row)
    println(row.grade)

    val rwth2 = rwth.map {
      case e if e.registrationNumber == "***REMOVED***" =>
        e.setGrade("9.3").setFamilyName(e.familyName.reverse)
    }

    val row2 = rwth2.byRegistrationNumber("***REMOVED***")
    println(row2)
    println(row2.grade)

    rwth2.save(Path.of("/tmp/output.csv"))
  }
}
