package assessments.pageelements

import assessments.*
import com.eed3si9n.eval.Eval
import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.PyQuote
import play.api.libs.json.{JsObject, JsString, JsValue}
import utils.Tag
import utils.Tag.Tags

import scala.util.Using

sealed trait Element

class StaticElement extends Element

/** Potentially interactive elements on an assessment page. */
// TODO rename DynamicElement
trait PageElement extends Element { self =>
  val name: ElementName
  def renderHtml: Html
  def updateAction(assessment: Assessment, state: Map[ElementName, JsValue]): IterableOnce[ElementAction] = Seq.empty
  val tags: Tag.Tags[self.type]
  val initialState: JsValue = JsObject(collection.Seq("content" -> JsString("")))
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
object ElementAction {
  def error(message: String): ElementAction =
    ElementAction(ElementName.errordisplay, JsObject(Map("message" -> JsString(message))))
}
