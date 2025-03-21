package assessments.pageelements

import assessments.Assessment.{markdownParser, markdownRenderer, templateRegex}
import assessments.pageelements.MathPreviewElement.mathtextToLatex
import assessments.*
import com.eed3si9n.eval.Eval
import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.PyQuote
import play.api.libs.json.{JsObject, JsString, JsValue}

import scala.util.Using

/** Potentially interactive elements on an assessment page. */
trait PageElement {
  val name: ElementName
  def renderHtml: String
  def action(assessment: Assessment, payload: JsValue): (IterableOnce[ElementAction], Any) = (Seq.empty, ())
  def otherAction(assessment: Assessment, element: PageElement, data: Any, payload: JsValue): IterableOnce[ElementAction] = Seq.empty
}

object PageElement {
  private val eval = new Eval(mkReporter = None, nonCpOptions = Seq("-experimental"), classpath = Eval.currentClasspath, backingDir = None)

  private val preamble =
    """import scala.language.experimental.genericNumberLiterals
      |import assessments.stack.{StackMath, StackUtils, StackParser}
      |import assessments.{ElementName}
      |import assessments.Preamble._
      |""".stripMargin

  def parsePageElement(spec: String): PageElement = {
    val (nameRaw, code) = {
      val index = spec.indexOf(':')
      if (index == -1)
        throw new SyntaxError(s"Could not parse tag `$spec`, missing `:`")
      (spec.substring(0, index).trim, spec.substring(index + 1).trim)
    }
    val name = ElementName(nameRaw)
    Using.resource(Context(name)) { context =>
      val augmentedCode = preamble
        + s"given assessments.Context = ${context.getterCode};\n"
        + code
//      Eval[PageElement](augmentedCode)
//      println(classOf[PageElement].getName)
//      println(augmentedCode)
      val pageElement = eval.eval(augmentedCode, Some(classOf[PageElement].getName))
        .getValue(getClass.getClassLoader)
        .asInstanceOf[PageElement]
      assert(pageElement.name == name)
      pageElement
    }
  }
}

case class ElementAction(element: ElementName, data: JsValue)








