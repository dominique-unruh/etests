package assessments.stack

import assessments.math.Math
import assessments.{ElementName, ExceptionContext, ExceptionWithContext, Html, SyntaxError}
import assessments.pageelements.InputElement
import assessments.math.Math.{Operation, Ops}
import com.typesafe.scalalogging.Logger
import externalsystems.MoodleStack
import externalsystems.MoodleStack.{Question, Quiz, inputElementToMoodle}
import ujson.{Arr, Str, transform}
import utils.{Docker, Utils, memoized}
import utils.Tag.Tags
import utils.Utils.awaitResult
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.*
import sttp.client4.*
import sttp.client4.circe.*

import java.nio.file.Path
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object StackParser {

  /** Typed request/response for the stack-parser HTTP service (`POST <stackparser.url>/parse`).
   * `result` is the pseudo-LaTeX JSON tree (old result.txt); `errors` is the validation error
   * string or null (old errors.txt). */
  private case class ParseRequest(expression: String, questionXml: String)
  private object ParseRequest { given Encoder[ParseRequest] = deriveEncoder }
  private case class ParseResponse(result: Option[String], errors: Option[String])
  private object ParseResponse { given Decoder[ParseResponse] = deriveDecoder }

  sealed trait MaximaTerm
  case class MaximaSymbol(name: String) extends MaximaTerm
  case class MaximaAtom(name: String) extends MaximaTerm
  case class MaximaInteger(int: BigInt) extends MaximaTerm
  case class MaximaOperation(head: MaximaTerm, args: MaximaTerm*) extends MaximaTerm

  def parseArray(json: ujson.Value): MaximaTerm = json match
    case Arr(ArrayBuffer(Str("operation"), head, args*)) =>
      MaximaOperation(parseArray(head), args.map(a => parseArray(a)) *)
    case Arr(ArrayBuffer(Str("atom"), Str(name))) =>
      MaximaAtom(name)
    case Arr(ArrayBuffer(Str("symbol"), Str(name))) =>
      MaximaSymbol(name)
    case Arr(ArrayBuffer(Str("integer"), Str(int))) =>
      MaximaInteger(BigInt(int))
    case _ => throw RuntimeException(s"Invalid json found coming from maxima: $json")

  def maximaToStackMath(maximaTerm: MaximaTerm)(using exceptionContext: ExceptionContext): Math = {
    def to(term: MaximaTerm): Math = term match
      case MaximaSymbol(name) =>
        if (!name.startsWith("%"))
          Math.Variable(name)
        else name match
          case "%i" => Math.imaginaryUnit
          case "%e" => Math.eulerConstant
          case "%pi" => Math.pi
          case _ => throw ExceptionWithContext(s"Unknown maxima special symbol '$name' encountered in $maximaTerm")
      case MaximaAtom(name) =>
        if (name.startsWith("\"") && name.endsWith("\"")) Math.StringLiteral(name.stripSuffix("\"").stripPrefix("\""))
        else throw ExceptionWithContext(s"Unknown maxima atom '$name' encountered in $maximaTerm")
      case MaximaInteger(int) => Math.Integer(int)
      case MaximaOperation(MaximaAtom(name), args*) =>
        val nameStripped = name.stripSuffix("\"").stripPrefix("\"")
        val (op, iter) = (nameStripped, args.length) match
          case ("+", 1) => (Ops.unaryPlus, false)
          case ("+", n) if n > 1 => (Ops.plus, true)
          case ("*", n) if n > 1 => (Ops.times, true)
          case ("/", 2) => (Ops.divide, false)
          case ("^", 2) => (Ops.power, false)
          case ("-", 1) => (Ops.unaryMinus, false)
          case ("xor", n) if n > 1 => (Ops.xor, true)
          case ("and", n) if n > 1 => (Ops.and, true)
          case ("nounand", n) if n > 1 => (Ops.and, true)
          case ("not", 1) => (Ops.not, false)
          case ("nounnot", 1) => (Ops.not, false)
          case ("or", n) if n > 1 => (Ops.or, true)
          case ("nounor", n) if n > 1 => (Ops.or, true)
          case (".", 2) => (Ops.times, false)
          case ("[", n) => (Ops.list, false)
          case ("{", n) => (Ops.set, false)
          case ("=", 2) => (Ops.equal, false)
          case ("!", 1) => (Ops.factorial, false)
          case _ => throw ExceptionWithContext(s"Unknown maxima atom \"$nameStripped\" of arity ${args.length} in $maximaTerm")
        if (iter)
          args.tail.foldLeft(to(args.head))((t, a) => Operation(op, t, to(a)))
        else
          Math.Operation(op, args.map(to) *)
      case MaximaOperation(MaximaSymbol(name), args*) =>
        Math.Funcall(name, args.map(to) *)
      case MaximaOperation(head, _*) =>
        throw RuntimeException(s"Maxima operation with unexpected head ${head}")

    to(maximaTerm)
  }

  @deprecated
  def parse(input: String)(using exceptionContext: ExceptionContext): Math = {
    val pageElement = InputElement(ElementName("input"), "reference", Tags())
    parse(input, pageElement)
  }

  def parse(expression: String, inputElement: InputElement)(using exceptionContext: ExceptionContext): Math =
    parseFuture(expression, inputElement).awaitResult()

  @memoized
  def parseFuture(expression: String, inputElement: InputElement)(using exceptionContext: ExceptionContext): Future[Math] = {
    if (expression.trim.isEmpty)
      throw SyntaxError("empty string is not a valid math expression")

    // A STACK `matrix` input reads per-cell sub-fields (`ans1_sub_i_j`) and needs its size derived from
    // the teacher answer via `adapt_to_model_answer`; the parsing harness does neither (it validates a
    // single `ans1` value), so a matrix input always yields an empty parse. A `varmatrix` input reads the
    // whole value as one string (rows split by newline, columns by whitespace) and needs no sizing, which
    // is exactly the grid format matrix answers arrive in. So parse matrix inputs as varmatrix.
    val inputMoodleRaw = inputElementToMoodle(inputElement.copy(ElementName("ans1")))
    val inputMoodle =
      if (inputMoodleRaw.typ == MoodleStack.InputType.matrix)
        inputMoodleRaw.copy(typ = MoodleStack.InputType.varmatrix)
      else
        inputMoodleRaw
    val quiz = Quiz(Question(name = "dummy",
      questionText = Html("dummy"),
      inputs = Seq(inputMoodle),
    ))
    val xml = quiz.prettyXml

    // Turn the STACK output — the pseudo-LaTeX JSON tree (`result` / `result.txt`) plus any error
    // string (`errors` / `errors.txt`) — into a `Math`. Shared by the service and Docker code paths so
    // both behave identically. `resultOpt == None` means STACK produced no result at all.
    def interpret(resultOpt: Option[String], errorsOpt: Option[String]): Math = {
      for (errors <- errorsOpt) {
        if (errors.contains("CAS failed to return any data due to timeout"))
          // Don't throw SyntaxError in this case, since SyntaxError implies that it's the fault of the `expression`.
          throw RuntimeException(s"Error parsing $expression: $errors")
        throw SyntaxError(s"Error parsing $expression: $errors")
      }
      val pseudoLatex = resultOpt.getOrElse(
        throw RuntimeException("Could not parse with Stack, unknown reason"))
      if (pseudoLatex.isEmpty)
        throw SyntaxError("Not parseable by Stack (unknown reason, stack gave empty parse result). This can happen if an expression of wrong number of lines is parsed as a fixed size matrix.")
      assert(pseudoLatex.startsWith("\\[ "), pseudoLatex)
      assert(pseudoLatex.endsWith(" \\]"), pseudoLatex)
      val json = pseudoLatex.stripPrefix("\\[ ").stripSuffix(" \\]")
      val maximaTerm = parseArray(ujson.read(json))
      maximaToStackMath(maximaTerm)
    }

    Utils.getSystemPropertyOptional("stackparser.url",
        "base URL of the stack-parser HTTP service, e.g. http://localhost:8080; if unset, a one-shot Docker container is used per parse") match {
      case Some(baseUrl) =>
        // Service path: POST a typed ParseRequest to the warm stack-parser daemon and read back a
        // typed ParseResponse ({result, errors}, equal to the old result.txt / errors.txt).
        basicRequest
          .post(uri"${baseUrl.stripSuffix("/")}/parse")
          .contentType("application/json")
          .body(ParseRequest(expression, xml).asJson.noSpaces)
          .response(asJson[ParseResponse])
          .send(backend)
          .map {
            _.body match {
              case Right(parsed) =>
                interpret(parsed.result, parsed.errors.filter(_.nonEmpty))
              case Left(error) =>
                throw RuntimeException(
                  s"stack-parser service failed for '$expression': ${error.getMessage}")
            }
          }

      case None =>
        // Legacy path: spin up a one-shot Docker container per parse.
        Docker.runInDocker(
          shortDescription = s"Parse $expression",
          image = Path.of("docker/stack-parser"),
          command = Seq("bash", "/parse.sh"),
          files = Map("expression.txt" -> expression, "question.xml" -> xml),
          requestedOutputs = Seq("result.txt", "errors.txt")).map { result =>
          if (result.exitCode != 0) {
            logger.debug(result.output)
            throw RuntimeException("Docker failed")
          }
          interpret(
            resultOpt = result.fileString("result.txt"),
            errorsOpt = result.fileString("errors.txt").filter(_.nonEmpty))
        }
    }
  }

  private val logger = Logger[this.type]
  private lazy val backend = DefaultFutureBackend()
}
