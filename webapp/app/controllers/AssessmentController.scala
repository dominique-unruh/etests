package controllers

import javax.inject.*
import play.api.*
import play.api.mvc.*

import java.nio.file.{Files, Path}
import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer

import javax.script.{ScriptEngine, ScriptEngineManager}
import scala.util.matching.Regex
import assessments.{Assessment, ElementName, IndentedInterpolator}
import play.api.libs.json.{JsArray, JsBoolean, JsString, JsValue, JsObject}
import play.mvc.BodyParser.Json
import play.twirl.api.Html


@Singleton
class AssessmentController @Inject()(val controllerComponents: ControllerComponents) extends BaseController {


/*
  private def replacer(m: Regex.Match) : String = {
    val code = m.group(1)
//    val result = Eval[AnyRef](code)
//    result.toString
//    import scala.reflect.runtime.universe as ru
//    import scala.tools.reflect.ToolBox
//    val scalaCode = toolbox.parse("""println("Hello world!")""")
//    val evalMirror = ru.runtimeMirror(this.getClass.getClassLoader)

//    val toolBox = evalMirror.mkToolBox()


//    toolBox.eval(scalaCode) //Hello world

    val cl = ClassLoader.getSystemClassLoader
    val x = getClass.getClassLoader
    val manager = new javax.script.ScriptEngineManager(application.classloader)
    val engine = manager.getEngineByName("scala")
//    manager.getEngineFactories.toString
//    engine.toString



    val srt = application.classloader.loadClass("scala.runtime.ScalaRunTime")
//
//    srt.toString
//    engine.eval("1").toString

//    val result = ammonite.Main().runCode("1")
//    result.toString()

//    val res = Eval().eval("1", Some("Int"))
//    Eval[String](""" "1" """)

//    val cl = getClass.getClassLoader
//    val clazz = cl.loadClass("controllers.EvalWrapper")
//    clazz.newInstance()
//    clazz.getTypeName
//    System.getProperty("java.class.path")
    code.toUpperCase
  }
*/

  private val exampleAssessmentMarkdown: String = Files.readString(Path.of("/home/unruh/r/assessments/data/test.md"))
  private val exampleAssessment: Assessment = Assessment.fromMarkdown(exampleAssessmentMarkdown)

  def assessment(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    val body = exampleAssessment.renderHtml()
    val html = views.html.assessment("Example", Html(body))
    Ok(html)
  }

  case class ElementAction(element: ElementName, data: JsValue) {
    def asJson: JsObject = JsObject(Seq(
      "element" -> JsString(element.toString),
      "callback" -> JsString(element.jsElementCallbackName),
      "data" -> data))
  }

  def elementAction(element: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    val payload = request.body.asJson.get
//    println((element, payload))
    val content = payload.asInstanceOf[JsObject].value("content").asInstanceOf[JsString]
    val actions = Seq(ElementAction(ElementName("test_input.preview"),
      JsObject(Seq("preview" -> content))))
    val response: JsValue = JsArray(actions.map(_.asJson))
    Ok(response)
  }
}
