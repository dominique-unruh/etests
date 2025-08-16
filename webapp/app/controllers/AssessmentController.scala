package controllers

import assessments.pageelements.{AnswerElement, ElementAction}

import javax.inject.*
import play.api.*
import play.api.mvc.*

import java.nio.file.{Files, Path}
import javax.script.{ScriptEngine, ScriptEngineManager}
import scala.util.matching.Regex
import assessments.{Assessment, ElementName, ExceptionContext, MarkdownAssessment}
import exam.y2025.iqc1.{Iqc1Exam, Uf}
import play.api.libs.json.{JsArray, JsBoolean, JsObject, JsString, JsValue}
import play.mvc.BodyParser.Json
import play.twirl.api.{Html, HtmlFormat}
import com.typesafe.scalalogging.Logger
import exam.y2025.iqc2.CnotConstruction
import externalsystems.Dynexite
import io.github.classgraph.{ClassGraph, ClassInfoList}
import org.apache.commons.lang3.exception.ExceptionUtils
import org.apache.commons.text.StringEscapeUtils
import utils.IndentedInterpolator

import scala.annotation.experimental
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.reflect.ClassTag
import scala.util.Random


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
//    logger.debug (module.getClass.toString)
    module.asInstanceOf[MarkdownAssessment].assessment
  }

  def assessment(assessmentName: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    packageContent(assessmentName) match {
      case Nil =>
        val assessment = getAssessment(assessmentName)
        val (body, explanation, gradingRules, files) = assessment.renderHtml
        val html = views.html.assessment(
          assessmentName = assessmentName,
          title = assessment.name,
          initialState = JsObject(assessment.pageElements.map{ (name, element) => (name.toString, element.initialState) }),
          reachablePoints = assessment.reachablePoints.decimalFractionString,
          body = Html(body.html),
          explanation = Html(explanation.html),
          gradingRules = Html(gradingRules.html))
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

  def loadReference(assessmentName: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    val assessment = getAssessment(assessmentName)
    given ExceptionContext = ExceptionContext.initialExceptionContext("Responding to web-query for reference solution from Dynexite exam",
      assessmentName)
    // TODO: Don't hardcode exam!
    val answers = assessment.referenceSolution
    Ok(answersToActions(assessment, answers))
  }

  private def answersToActions(assessment: Assessment, answers: Map[ElementName, String]): JsArray =
    JsArray(
      for ((element, content) <- answers.toSeq;
           action <- assessment.pageElements(element).asInstanceOf[AnswerElement].setAction(content))
      yield elementActionAsJson(action))

  def loadAnswers(assessmentName: String, registrationNumber: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      val assessment = getAssessment(assessmentName)
      given ExceptionContext = ExceptionContext.initialExceptionContext("Responding to web-query for student answers from Dynexite exam", assessmentName, registrationNumber)
      val answers = Dynexite.getDynexiteAnswers(assessment = assessment, exam = Iqc1Exam, registrationNumber = registrationNumber)
      assert(answers.forall(_._2 != null))
      Ok(answersToActions(assessment, answers))
    } catch {
      case e: Throwable =>
        Ok(JsArray(Seq(elementActionAsJson(ElementAction.error(ExceptionUtils.getStackTrace(e))))))
    }
  }

  def assessmentFile(assessmentName: String, fileName: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    val assessment = getAssessment(assessmentName)
    val (body, explanation, gradingRules, files) = assessment.renderHtml
    val (mime, content) = files(fileName)
    Ok(content).as(mime)
  }

  def updateAction(assessmentName: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    // TODO do some caching
    val assessment = getAssessment(assessmentName)
    val payload = request.body.asJson.get.asInstanceOf[JsObject]
    val actions = assessment.updateAction(payload)
    val response: JsValue = JsArray(actions.map(elementActionAsJson))
    Ok(response)
  }

  private def elementActionAsJson(action: ElementAction) =
    JsObject(Seq(
      "element" -> JsString(action.element.toString),
      "callback" -> JsString(action.element.jsElementCallbackName),
      "data" -> action.data))

  private lazy val learners = Dynexite.resultsByLearner.view.collect({ case (regno, Some(_)) => regno }).toVector
  def randomStudent(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    val index = Random.nextInt(learners.length)
    Ok(JsObject(Map("registration" -> JsString(learners(index)))))
  }

  def dynexiteAnswers(assessmentName: String, regno: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    given ExceptionContext = ExceptionContext.initialExceptionContext(s"Responsing to web-request $request")
    val assessment = getAssessment(assessmentName)
    val result = StringBuilder()
    try {
      result ++= Dynexite.getDynexiteAnswersRaw(assessment, Iqc1Exam, regno).toString
    } catch
      case e: Throwable => result ++= ExceptionUtils.getStackTrace(e)
    result ++= "\n\n\n"
    try {
      result ++= Dynexite.getDynexiteAnswers(assessment, Iqc1Exam, regno).toString
    } catch
      case e: Throwable => result ++= ExceptionUtils.getStackTrace(e)

    Ok(result.result())
  }

  def dynexitePdf(regno: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    val pdf = Dynexite.getAnswerPDF(registrationNumber = regno)
    Ok(pdf).as("application/pdf")
  }

  def dynexiteLink(regno: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    Redirect(Dynexite.getLinkForLearner(regno))
  }
}
