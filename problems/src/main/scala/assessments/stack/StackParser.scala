package assessments.stack

import assessments.SyntaxError
import assessments.stack.StackMath.{Operation, Ops}
import ujson.{Arr, Str, transform}
import utils.Docker

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

  def maximaToStackMath(maximaTerm: MaximaTerm): StackMath = maximaTerm match
    case MaximaSymbol(name) => StackMath.Variable(name)
    case MaximaAtom(name) => ???
    case MaximaInteger(int) => StackMath.Integer(int)
    case MaximaOperation(MaximaAtom(name), args*) =>
      val nameStripped = name.stripSuffix("\"").stripPrefix("\"")
      val (op,iter) = (nameStripped, args.length) match
        case ("+", 1) => (Ops.unaryPlus, false)
        case ("+", n) if n > 1 => (Ops.plus, true)
        case ("*", n) if n > 1 => (Ops.times, true)
        case ("/", 2) => (Ops.divide, false)
        case ("^", 2) => (Ops.power, false)
        case ("-", 1) => (Ops.unaryMinus, false)
        case ("xor", n) if n > 1 => (Ops.xor, true)
        case ("and", n) if n > 1 => (Ops.and, true)
        case ("not", 1) => (Ops.not, true)
        case ("or", n) if n > 1 => (Ops.or, true)
        case (".", 2) => (Ops.times, false)
        case _ => throw RuntimeException(s"Unknown maxima atom \"$nameStripped\" of arity ${args.length}")
      if (iter)
        args.tail.foldLeft(maximaToStackMath(args.head))((t,a) => Operation(op, t, maximaToStackMath(a)))
      else
        StackMath.Operation(op, args.map(maximaToStackMath)*)
    case MaximaOperation(MaximaSymbol(name), args*) =>
      StackMath.Funcall(name, args.map(maximaToStackMath)*)

  def parse(input: String): StackMath = {
    if (input.contains(';'))
      throw SyntaxError("; not allowed in Stack math")
    if (input.trim.isEmpty)
      throw SyntaxError("empty string is not a valid math expression")

    val result = Docker.runInDocker(Path.of("docker/stack-parser"),
      Seq("bash", "/parse.sh"),
      files = Map("expression.txt" -> input), requestedOutputs = Seq("result.txt"))

//    println(result.exitCode)
//    println(result.files.view.mapValues(new String(_)).toMap)
    if (result.exitCode != 0)
      throw RuntimeException("Docker failed")
//    if (result.fileString("status.txt").getOrElse("").contains("parsing"))
//      throw SyntaxError(s"Could not parse $inputFixed using maxima")
    if (!result.files.contains("result.txt"))
      throw RuntimeException("Could not parse with maxima, unknown reason")
    val pseudoLatex = result.fileString("result.txt").get
    assert(pseudoLatex.startsWith("\\[ "))
    assert(pseudoLatex.endsWith(" \\]"))
    val json = pseudoLatex.stripPrefix("\\[ ").stripSuffix(" \\]")
    val array = ujson.read(json)

    val maximaTerm = parseArray(array)

    maximaToStackMath(maximaTerm)
  }

}
