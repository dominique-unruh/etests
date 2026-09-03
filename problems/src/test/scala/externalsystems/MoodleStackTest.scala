package externalsystems

import assessments.ElementName
import assessments.Html
import assessments.pageelements.InputElement
import externalsystems.MoodleStack.{FunctionDefinition, InputType, InsertStars, defaultForbiddenWords, moodleValidation}
import org.scalatest.funsuite.AnyFunSuiteLike
import utils.Tag.Tags

class MoodleStackTest extends AnyFunSuiteLike {
  test("defaultForbiddenWords") {
    println(defaultForbiddenWords.toSeq.sorted)
    assert(defaultForbiddenWords.contains("Y"))
    assert(defaultForbiddenWords.contains("xP"))
  }

  private def input(name: String, questionVariables: Seq[String] = Seq.empty): MoodleStack.Input =
    MoodleStack.Input(
      typ = InputType.algebraic, name = name, reference = "?",
      forbidWords = Seq.empty, allowWords = Seq.empty, extraOptions = Seq.empty,
      insertStars = InsertStars.dontInsert, boxsize = 15,
      questionVariables = questionVariables)

  test("moodleValidation emits validator option and injects helper definitions") {
    val sub = FunctionDefinition("subhelper", Seq.empty, Seq("subhelper(x) := x;"))
    val helper = FunctionDefinition("myval", Seq(sub), Seq("""myval(ans) := if subhelper(ans) # 0 then "" else "bad";"""))
    val element = InputElement(ElementName("ans1"), reference = "42",
      tags = Tags[InputElement](moodleValidation := helper))
    val moodleInput = MoodleStack.inputElementToMoodle(element)

    // sub-helper definitions come first (transitive), then the validator's own definition.
    assert(moodleInput.questionVariables == Seq("subhelper(x) := x;", """myval(ans) := if subhelper(ans) # 0 then "" else "bad";"""))
    assert(moodleInput.extraOptions.exists(_ == "validator:myval"))
    assert(moodleInput.xml.toString.contains("validator:myval"))
  }

  test("no moodleValidation tag => no validator") {
    val element = InputElement(ElementName("ans1"), reference = "42",
      tags = Tags[InputElement]())
    val moodleInput = MoodleStack.inputElementToMoodle(element)

    assert(moodleInput.questionVariables.isEmpty)
    assert(!moodleInput.extraOptions.exists(_.startsWith("validator:")))
  }

  test("question merges its own and inputs' variables") {
    val question = MoodleStack.Question(name = "q", questionText = Html("txt"),
      questionVariables = Seq("base : 1;"),
      inputs = Seq(input("ans1", Seq("validator_ans1(ans) := \"\";"))))
    val xml = question.xml.toString
    assert(xml.contains("base : 1;"))
    assert(xml.contains("validator_ans1(ans) := \"\";"))
  }

  test("identical shared helper definitions are deduplicated") {
    val helper = "helper(a) := a + 1;"
    val question = MoodleStack.Question(name = "q", questionText = Html("txt"),
      inputs = Seq(input("ans1", Seq(helper)), input("ans2", Seq(helper))))
    val varsBlock = (question.xml \\ "questionvariables" \ "text").text
    assert(varsBlock.split("\n").count(_.contains(helper)) == 1)
  }

  test("conflicting definitions of the same variable are an error") {
    val question = MoodleStack.Question(name = "q", questionText = Html("txt"),
      inputs = Seq(input("ans1", Seq("f(a) := a;")), input("ans2", Seq("f(a) := a + 1;"))))
    assertThrows[IllegalArgumentException](question.xml)
  }
}
