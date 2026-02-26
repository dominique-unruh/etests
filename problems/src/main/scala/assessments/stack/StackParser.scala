package assessments.stack

import assessments.{ElementName, Html, SyntaxError}
import assessments.pageelements.InputElement
import assessments.stack.StackMath.{Operation, Ops}
import externalsystems.MoodleStack.{Question, Quiz, inputElementToMoodle}
import ujson.{Arr, Str, transform}
import utils.Docker
import utils.Tag.Tags

import java.nio.file.Path
import scala.collection.mutable.ArrayBuffer

object StackParser {

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

  def maximaToStackMath(maximaTerm: MaximaTerm): StackMath = {
    def to(term: MaximaTerm): StackMath = term match
      case MaximaSymbol(name) =>
        if (!name.startsWith("%"))
          StackMath.Variable(name)
        else name match
          case "%i" => StackMath.imaginaryUnit
          case "%e" => StackMath.eulerConstant
          case "%pi" => StackMath.pi
          case _ => throw RuntimeException(s"Unknown maxima special symbol '$name' encountered in $maximaTerm")
      case MaximaAtom(name) => ???
      case MaximaInteger(int) => StackMath.Integer(int)
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
          case ("not", 1) => (Ops.not, true)
          case ("or", n) if n > 1 => (Ops.or, true)
          case (".", 2) => (Ops.times, false)
          case ("[", n) => (Ops.list, false)
          case ("=", 2) => (Ops.equal, false)
          case _ => throw RuntimeException(s"Unknown maxima atom \"$nameStripped\" of arity ${args.length} in $maximaTerm")
        if (iter)
          args.tail.foldLeft(to(args.head))((t, a) => Operation(op, t, to(a)))
        else
          StackMath.Operation(op, args.map(to) *)
      case MaximaOperation(MaximaSymbol(name), args*) =>
        StackMath.Funcall(name, args.map(to) *)

    to(maximaTerm)
  }

  @deprecated
  def parse(input: String): StackMath = {
    val pageElement = InputElement(ElementName("input"), "reference", Tags())
    parse(input, pageElement)
  }

  def parse(expression: String, inputElement: InputElement): StackMath = {
    if (expression.trim.isEmpty)
      throw SyntaxError("empty string is not a valid math expression")

    val inputMoodle = inputElementToMoodle(inputElement.copy(ElementName("ans1")))
    val quiz = Quiz(Question(name = "dummy",
      questionText = Html("dummy"),
      inputs = Seq(inputMoodle),
    ))
    val xml = quiz.prettyXml

    val result = Docker.runInDocker(Path.of("docker/stack-parser"),
      Seq("bash", "/parse.sh"),
      files=Map("expression.txt" -> expression, "question.xml" -> xml),
      requestedOutputs = Seq("result.txt", "errors.txt"))

    if (result.exitCode != 0)
      throw RuntimeException("Docker failed")
//    if (result.fileString("status.txt").getOrElse("").contains("parsing"))
//      throw SyntaxError(s"Could not parse $inputFixed using maxima")
    for (errors <- result.fileString("errors.txt"))
      throw SyntaxError(s"Error parsing $expression: $errors")
    if (!result.files.contains("result.txt"))
      throw RuntimeException("Could not parse with Stack, unknown reason")
    val pseudoLatex = result.fileString("result.txt").get
    if (pseudoLatex.isEmpty)
      throw SyntaxError("Not parseable by Stack (unknown reason, stack gave empty parse result). This can happen if an expression of wrong number of lines is parsed as a fixed size matrix.")
    assert(pseudoLatex.startsWith("\\[ "), pseudoLatex)
    assert(pseudoLatex.endsWith(" \\]"), pseudoLatex)
    val json = pseudoLatex.stripPrefix("\\[ ").stripSuffix(" \\]")
    val array = ujson.read(json)

    val maximaTerm = parseArray(array)

    maximaToStackMath(maximaTerm)
  }

}
