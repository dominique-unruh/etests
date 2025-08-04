package controllers

import assessments.pageelements.ElementAction

import javax.inject.*
import play.api.*
import play.api.mvc.*

import java.nio.file.{Files, Path}
import javax.script.{ScriptEngine, ScriptEngineManager}
import scala.util.matching.Regex
import assessments.{Assessment, ElementName, ExceptionContext, MarkdownAssessment}
import exam.y2025.iqc1.CnotConstruction
import play.api.libs.json.{JsArray, JsBoolean, JsObject, JsString, JsValue}
import play.mvc.BodyParser.Json
import play.twirl.api.Html
import com.typesafe.scalalogging.Logger
import io.github.classgraph.{ClassGraph, ClassInfoList}
import org.apache.commons.text.StringEscapeUtils
import utils.IndentedInterpolator

import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.reflect.ClassTag


@Singleton
class AssessmentController @Inject()(val controllerComponents: ControllerComponents) extends BaseController {
  private val logger = Logger[AssessmentController]


//  private val exampleAssessmentMarkdown: String = Files.readString(Path.of("/home/unruh/r/assessments/data/test.md"))
//  private val exampleAssessment: Assessment = CnotConstruction.assessment

  private lazy val classgraph = new ClassGraph()
    .enableClassInfo()
    .acceptPackages("exam")
    .scan()

  private def packageContent(name: String) = {
    classgraph.getPackageInfo(name) match {
      case null => Nil
      case pkg =>
        val results = Seq.newBuilder[String]
        for (clazz <- pkg.getClassInfo.asScala
             if clazz.extendsSuperclass(classOf[MarkdownAssessment]))
          results += clazz.getName.stripSuffix("$")
        results.result()
    }
  }

  private def getAssessment(name: String) = {
    val clazz = Class.forName (name.stripSuffix ("/").replace ('/', '.') + "$")
    val moduleField = clazz.getField ("MODULE$")
    val module = moduleField.get (null)
    logger.debug (module.getClass.toString)
    module.asInstanceOf[MarkdownAssessment].assessment
  }

  def assessment(assessmentName: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    packageContent(assessmentName) match {
      case Nil =>
        val assessment = getAssessment(assessmentName)
        val (body,files) = assessment.renderHtml
        val html = views.html.assessment(assessment.name, Html(body))
        Ok(html)
      case packageContent =>
        val html = StringBuilder()
        html ++= "<ul>\n"
        for (name <- packageContent) {
          val escapedName = StringEscapeUtils.escapeHtml4(name)
          val link = routes.AssessmentController.assessment(name).url
          html ++= s"""  <li><a href="$link">$escapedName</a></li>\n"""
        }
        html ++= "</ul>\n"
        Ok(Html(html.result()))
    }
  }

  def assessmentFile(assessmentName: String, fileName: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    val assessment = getAssessment(assessmentName)
    val (body,files) = assessment.renderHtml
    val (mime, content) = files(fileName)
    Ok(content).as(mime)
  }

  private def elementActionAsJson(action: ElementAction) =
    JsObject(Seq(
      "element" -> JsString(action.element.toString),
      "callback" -> JsString(action.element.jsElementCallbackName),
      "data" -> action.data))

  def elementEvent(assessmentName: String, element: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    val assessment = getAssessment(assessmentName)
    val payload = request.body.asJson.get
    val actions = assessment.elementEvent(ElementName(element), payload)
    val response: JsValue = JsArray(actions.map(elementActionAsJson))
    Ok(response)
  }
}
