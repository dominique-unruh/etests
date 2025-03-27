package assessments.stack

import assessments.SyntaxError
import assessments.stack.StackMath.{Bool, Funcall, Integer, Operation, Ops, Variable}


object StackParser {

  import fastparse.*
  import MultiLineWhitespace.*

  //noinspection TypeAnnotation
  object Strings {
    inline val lower_case_letters = "abcdefghijklmnopqrstuvwxyz"
    inline val upper_case_letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    inline val letters = lower_case_letters + upper_case_letters
    inline val digits = "0123456789"
    inline val letters_digits = letters + digits
    inline val letters_digits_underscore = letters_digits + "_"
  }

  def anyOf[A, $: P](parsers: IterableOnce[() => P[A]]): P[A] = {
    var parser: P[A] = null
    for (p <- parsers) {
      if (parser == null)
        parser = p()
      else
        parser = parser | p()
    }
    assert(parser != null)
    parser
  }

  def selector[A, $: P](map: (String, A)*): P[A] =
    anyOf(map.iterator.map((keyword, value) => () => LiteralStr(keyword).map(_ => value)))

  def infixl_rep[I, A, $: P](argument: => P[A], infix: => P[I], f: (A, I, A) => A): P[A] = {
    (argument ~ (infix ~ argument).rep) map { (arg1, ops) =>
      var result = arg1
      for ((op, arg) <- ops)
        result = f(result, op, arg)
      result
    }
  }

  def identifier[$: P]: P[String] = P((CharIn(Strings.letters) ~ CharsWhileIn(Strings.letters_digits_underscore, 0)).!)

  def variable[$: P]: P[StackMath] = P(identifier.map(Variable.apply))

  def integer[$: P]: P[StackMath] = P(CharsWhileIn(Strings.digits, 1).!.map(i => Integer(i.toInt)))

  def function_application[$: P]: P[StackMath] = P((identifier ~ "(" ~ num_expr.rep(sep = ",") ~ ")")
    .map { case (head: String, args: Seq[StackMath]) => Funcall(head, args *) })

  def atom[$: P]: P[StackMath] = P("(" ~ num_expr ~ ")" | function_application | variable | integer)

  def power_like[$: P]: P[StackMath] = P((atom ~ "^" ~ atom).map((x, y) => Operation(Ops.power, x, y)) | atom)


  def mult_like[$: P]: P[StackMath] =
    P(infixl_rep_map(power_like, "*" -> Ops.times, "/" -> Ops.divide))

  def infixl_rep_map[$: P](argument: => P[StackMath], map: (String, Ops)*): P[StackMath] =
    infixl_rep(argument, selector(map *), { (a, o, b) => Operation(o, a, b) })

  def unary_add_like[$: P]: P[StackMath] = mult_like

  def add_like[$: P]: P[StackMath] = P(
    infixl_rep_map(unary_add_like, "+" -> Ops.plus, "-" -> Ops.minus))

  def comparison_ops: Seq[(String, Ops)] =
    Seq("=" -> Ops.equal, "<=" -> Ops.less_eq, "=>" -> Ops.greater_eq,
      ">" -> Ops.greater, "<" -> Ops.less)

  def comparison[$: P]: P[StackMath] = P {
    (add_like ~ (selector(comparison_ops *) ~ add_like).?) map {
      case (e, None) => e
      case (e, Some(op, e2)) => Operation(op, e, e2)
    }
  }

  def boolean[$: P]: P[Bool] = selector("true" -> Bool(true), "false" -> Bool(false))

  def bool_atom[$: P]: P[StackMath] = ("(" ~ bool_expr ~ ")") | comparison | variable | boolean

  def not_like[$: P]: P[StackMath] = P((LiteralStr("not") ~/ not_like).map(x => Operation(Ops.not, x)) | bool_atom)

  def and_like[$: P]: P[StackMath] = infixl_rep_map(not_like, "and" -> Ops.and)

  def or_like[$: P]: P[StackMath] = infixl_rep_map(not_like, "or" -> Ops.or, "xor" -> Ops.xor)

  def num_expr[$: P]: P[StackMath] = P(add_like)

  def bool_expr[$: P]: P[StackMath] = P(or_like)

  def expr[$: P]: P[StackMath] = num_expr
//  def expr[$: P]: P[StackMath] = bool_expr | num_expr

  def parseWith[A](input: String, parser: fastparse.ParsingRun[?] ?=> fastparse.ParsingRun[A]): A =
    import fastparse.*
    import MultiLineWhitespace.*
    fastparse.parse(input, { (p: P[?]) => given P[?] = p; parser ~ End }, verboseFailures = true) match {
      case Parsed.Success(value, index) => value
      case failure: Parsed.Failure => throw SyntaxError(failure.msg)
    }

  def parse(input: String): StackMath = parseWith(input, comparison | expr)
}
