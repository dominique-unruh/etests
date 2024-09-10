import assessments.Assessment
import assessments.Assessment.{tagEnd, tagStart}
import org.scalatest.funsuite.AnyFunSuiteLike

import scala.collection.immutable.HashMap

class AssessmentTest extends AnyFunSuiteLike {
  test("experiment") {
//    val inputName = ElementName("input1")
//    val input: PageElement = new PageElement {
//      val name: ElementName = inputName
//      override def renderHtml: String = s"""<input type="text" id="$name"/>"""
//    }
//
//    val template =
//      s"""# Problem 1
//         |Input: ${tagStart}${inputName}${tagEnd}""".stripMargin

//    val pageElements = HashMap(
//      inputName -> input
//    )

    val template =
      """# Problem 1
         |Input: {{inputfield}}""".stripMargin

    val assessment = Assessment.fromMarkdown(template)

    val html = assessment.renderHtml()

    println(html)
  }
}
