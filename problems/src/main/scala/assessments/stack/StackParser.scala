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

    // TODO This parser is still not compatible with Dynexite/STACK.
    // Better: run the whole PHP parser (it does some additional heuristic fixing etc)
    // See class stack_algebraic_input maybe in stack source as starting point (https://github.com/maths/moodle-qtype_stack.git) and api/README.md for setup maybe.
    val inputFixed = input.replace('÷', '/').replace('×','*')
    
    val result = Docker.runInDocker( // synchronized to avoid too many parallel processes
      image = Path.of("docker/maxima-parser"),
      command = Seq("maxima", "-b", "/parse.mac"),
      files = Map("expression.txt" -> inputFixed),
      requestedOutputs = Seq("result.txt", "status.txt")
    )
//    println(result.exitCode)
//    println(result.files.view.mapValues(new String(_)).toMap)
    if (result.exitCode != 0)
      throw RuntimeException("Docker failed")
    if (result.fileString("status.txt").getOrElse("").contains("parsing"))
      throw SyntaxError(s"Could not parse $inputFixed using maxima")
    if (!result.files.contains("result.txt"))
      throw RuntimeException("Could not parse with maxima, unknown reason")
    val term = result.fileString("result.txt").get
    val term2 = term.trim.stripSuffix(";")
    val array = ujson.read(term2)

//    println(array)

    val maximaTerm = parseArray(array)

//    println(maximaTerm)
    maximaToStackMath(maximaTerm)
  }

}
