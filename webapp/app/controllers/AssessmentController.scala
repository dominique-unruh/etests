package controllers

import assessments.ExceptionContext.initialExceptionContext
import assessments.pageelements.{AnswerElement, ElementAction}

import javax.inject.*
import play.api.*
import play.api.mvc.*

import java.nio.file.{Files, Path}
import javax.script.{ScriptEngine, ScriptEngineManager}
import scala.util.matching.Regex
import assessments.{Assessment, ElementName, Exam, ExceptionContext, MarkdownAssessment}
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

  private def getAssessment(exam: Exam, name: String)(using exceptionContext: ExceptionContext): MarkdownAssessment = {
    given ExceptionContext = ExceptionContext.addToExceptionContext(s"Looking up question $name in exam $exam")
    exam.assessmentByName(name)
  }

  private def getExam(name: String) = {
    val clazz = Class.forName (name.stripSuffix ("/").replace ('/', '.') + "$")
    val moduleField = clazz.getField ("MODULE$")
    val module = moduleField.get (null)
    module.asInstanceOf[Exam]
  }

  def allExams(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    val exams = Exam.exams
    val html = StringBuilder()
    html ++= s"<h1>Available exams</h1>\n"
    html ++= "<ul>\n"
    for (exam <- exams.sortBy(_.name))
      val escapedName = StringEscapeUtils.escapeHtml4(exam.name)
      val link = routes.AssessmentController.exam(exam.getClass.getName.stripSuffix("$")).url
      html ++= s"""  <li><a href="$link">$escapedName</a> (${exam.getClass.getSimpleName.stripSuffix("$")})</li>\n"""
    html ++= "</ul>\n"
    Ok(Html(html.result()))
  }


  def exam(examName: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    val exam = getExam(examName)
    val html = StringBuilder()
    html ++= s"<h1>Exam ${exam.name}</h1>\n"
    html ++= "<ul>\n"
    for (problem <- exam.problems)
      val escapedName = StringEscapeUtils.escapeHtml4(problem.name)
      val link = routes.AssessmentController.assessment(examName, problem.name).url
      html ++= s"""  <li><a href="$link">$escapedName</a> (${problem.getClass.getSimpleName.stripSuffix("$")})</li>\n"""
    html ++= "</ul>\n"
    Ok(Html(html.result()))
  }

  def assessment(examName: String, assessmentName: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    given ExceptionContext = ExceptionContext.initialExceptionContext(s"Web query for exam $examName, problem $assessmentName")
    val exam = getExam(examName)
    val assessment = getAssessment(exam, assessmentName)
    val (body, explanation, gradingRules, files) = assessment.renderHtml
    val html = views.html.assessment(
      examName = examName,
      assessmentName = assessmentName,
      title = assessment.name,
      initialState = JsObject(assessment.pageElements.map{ (name, element) => (name.toString, element.initialState) }),
      reachablePoints = assessment.reachablePoints.decimalFractionString,
      body = Html(body.html),
      explanation = Html(explanation.html),
      gradingRules = Html(gradingRules.html))
    Ok(html)

  }

  def loadReference(examName: String, assessmentName: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    given ExceptionContext = ExceptionContext.initialExceptionContext("Responding to web-query for reference solution from Dynexite exam",
      assessmentName)
    val exam = getExam(examName)
    val assessment = getAssessment(exam, assessmentName)
    val answers = assessment.assessment.referenceSolution
    Ok(answersToActions(assessment, answers))
  }

  private def answersToActions(assessment: Assessment, answers: Map[ElementName, String]): JsArray =
    JsArray(
      for ((element, content) <- answers.toSeq;
           action <- assessment.pageElements(element).asInstanceOf[AnswerElement].setAction(content))
      yield elementActionAsJson(action))

  def loadAnswers(examName: String, assessmentName: String, registrationNumber: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    given ExceptionContext = ExceptionContext.initialExceptionContext("Responding to web-query for student answers from Dynexite exam", assessmentName, registrationNumber)
    try {
      val exam = getExam(examName)
      val assessment = getAssessment(exam, assessmentName)
      val answers = Dynexite.getDynexiteAnswers(assessment = assessment, exam = exam, registrationNumber = registrationNumber)
      assert(answers.forall(_._2 != null))
      Ok(answersToActions(assessment, answers))
    } catch {
      case e: Throwable =>
        Ok(JsArray(Seq(elementActionAsJson(ElementAction.error(ExceptionUtils.getStackTrace(e))))))
    }
  }

  def assessmentFile(examName: String, assessmentName: String, fileName: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    given ExceptionContext = initialExceptionContext(s"Responsing to web-request $request")
    val exam = getExam(examName)
    val assessment = getAssessment(exam, assessmentName)
    val (body, explanation, gradingRules, files) = assessment.renderHtml
    val (mime, content) = files(fileName)
    Ok(content).as(mime)
  }

  def updateAction(examName: String, assessmentName: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    given ExceptionContext = initialExceptionContext(s"Responsing to web-request $request")
    val exam = getExam(examName)
    // TODO do some caching
    val assessment = getAssessment(exam, assessmentName)
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

  def randomStudent(examName: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    val exam = getExam(examName)
    val learners = Dynexite.resultsByLearner(exam).view.collect({ case (regno, Some(_)) => regno }).toVector
    val index = Random.nextInt(learners.length)
    Ok(JsObject(Map("registration" -> JsString(learners(index)))))
  }

  def dynexiteAnswers(examName: String, assessmentName: String, regno: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    given ExceptionContext = ExceptionContext.initialExceptionContext(s"Responsing to web-request $request")
    val exam = getExam(examName)
    val assessment = getAssessment(exam, assessmentName)
    val result = StringBuilder()
    try {
      result ++= Dynexite.getDynexiteAnswersRaw(assessment, exam, regno).mkString("\n")
    } catch
      case e: Throwable => result ++= ExceptionUtils.getStackTrace(e)
    result ++= "\n\n\n"
    try {
      result ++= Dynexite.getDynexiteAnswers(assessment, exam, regno).toSeq.mkString("\n")
    } catch
      case e: Throwable => result ++= ExceptionUtils.getStackTrace(e)

    Ok(result.result())
  }

  def dynexitePdf(examName: String, regno: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    val exam = getExam(examName)
    val pdf = Dynexite.getAnswerPDF(exam, registrationNumber = regno)
    Ok(pdf).as("application/pdf")
  }

  def dynexiteLink(examName: String, regno: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    val exam = getExam(examName)
    Redirect(Dynexite.getLinkForLearner(exam, regno))
  }
}
