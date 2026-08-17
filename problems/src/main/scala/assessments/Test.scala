package assessments

import org.apache.commons.io.output.NullPrintStream
import sourcecode.{File, Line}
import utest.Tests
import utest.framework.{Result, TestCallTree, Tree}

import java.io.{ByteArrayOutputStream, OutputStream, PrintStream}
import java.rmi.server.LogStream

/** A single name-labelled test tree — the combined view of utest's two parallel structures
 * ([[utest.framework.Tree]] of names + [[utest.framework.TestCallTree]] of bodies), i.e. roughly a
 * `Tree[(name, runner)]`.
 *
 *   - A node with no `children` is a leaf: `run` is the test body, executed when the leaf runs.
 *   - A node with `children` is a group: `run` is setup, executed (each time) on the path down to a
 *     descendant leaf, then the children are entered.
 *
 * `declaredAt` (absolute `path:line`, captured via `sourcecode` at [[Test.apply]] / [[withName]]) is
 * printed when the node runs. utest/sbt carry no source position for a test, so the IDE can't drive
 * the test node's "jump to source"; but IDEs linkify an absolute `path:line` in the run console, so
 * printing it gives click-to-source to the declaration site.
 *
 * Convert to a utest suite with [[toTests]]. (It cannot itself `extend Tests`: `Tests` is a case
 * class, and a case class may not be extended by another case class.) */
final class Test(val name: String, val run: Option[ExceptionContext ?=> Unit],
                 val children: Seq[Test], val declaredAt: String) {
  assert(name != null)
  assert(name != "")
  assert(run.isEmpty || children.isEmpty)
  assert(declaredAt.nonEmpty)

  /** Print the declaration site (clickable in the run console) before running the body/setup. */
  private def announce(): Unit =
    println(s"$declaredAt: $name")

  private def nameTree: Tree[String] =
    Tree(name, children.map(_.nameTree) *)

  private def callTree(using exceptionContext: ExceptionContext): TestCallTree = {
//    WrapStream.wrapOut()
    run match {
      case Some(run) => new TestCallTree(Left {
        announce();
        run;
//        println(System.currentTimeMillis() % 100000)
//        System.out.flush()
//        System.err.flush()
//        Console.out.flush()
//        Thread.sleep(100)
        s"$name + $declaredAt"
      }) // leaf: forcing runs the body
      case None => new TestCallTree(Right { children.map(_.callTree).toIndexedSeq })
    }
  }

  /** Convert this tree into a utest suite value. */
  def toTests(using exceptionContext: ExceptionContext): Tests = Tests(nameTree, callTree)

  /** Execute the whole tree depth-first (group `run` as setup, then its children; a leaf's `run` is
   * the test body), propagating the first failure. Used for the headless self-test run
   * ([[MarkdownAssessment.runTests]] / `main`); the IDE and `sbt test` path uses [[toTests]] and
   * utest's own runner instead. */
  def runAll()(using exceptionContext: ExceptionContext): Unit = {
    given ExceptionContext =
      if (name.isEmpty) exceptionContext
      else ExceptionContext.addToExceptionContext(s"Running test '$name'")
    announce()
    run.foreach { r => r }
    children.foreach(_.runAll())
  }

  def appendChild(test: Test): Test = run match {
    case Some(run) =>
      new Test(name, None, Seq(this.withName("main"), test), declaredAt)
    case None => new Test(name, None, children.appended(test), declaredAt)
  }
  def withName(name: String): Test =
    new Test(name, run, children, declaredAt)
  def there(file: File, line: Line): Test =
    new Test(name, run, children, s"${file.value}:${line.value}")
  def here(using file: File, line: Line): Test = there(file, line)
}

object Test {
  def apply(name: String, children: Seq[Test] = Seq.empty)(using file: File, line: Line): Test =
    new Test(name, None, children, s"${file.value}:${line.value}")
  def apply(name: String)(run: ExceptionContext ?=> Unit)(using file: File, line: Line): Test =
    new Test(name, Some(run), Seq.empty, s"${file.value}:${line.value}")
}

/*
/** A [[PrintStream]] wrapper that rewrites every line before forwarding it: each `#` becomes `;;`.
 *
 * Install with `System.setOut(WrapStream(System.out))` (Scala can't assign `System.out` directly;
 * use `System.setOut`). Line-buffered: bytes are held until a newline (or an explicit `flush`),
 * then the transformed line — newline included — is forwarded to `underlying`. Works at the byte
 * level, so it is charset-agnostic (`#` = 0x23 never occurs inside a multi-byte UTF-8 sequence). */
class WrapStream(underlying: PrintStream)
  extends PrintStream(new WrapStream.LineRewriter(underlying), /* autoFlush = */ true)

object WrapStream {
  def apply(underlying: PrintStream): WrapStream = new WrapStream(underlying)

  def wrapOut() = System.out match {
    case stream: WrapStream =>
    case stream => System.setOut(WrapStream(stream))
  }

  private class LineRewriter(underlying: PrintStream) extends OutputStream {
    private val line = new ByteArrayOutputStream()

    override def write(b: Int): Unit = {
      line.write(b)
      if (b == '\n') flush()
    }

    /** Emit the buffered (partial or complete) line with `#` → `;;`, then flush the target. */
    override def flush(): Unit = {
      if (line.size() > 0) {
        for (b <- line.toByteArray)
          if (b == '#'.toByte) { underlying.write(';'); underlying.write(';') }
          else underlying.write(b & 0xFF)
        line.reset()
      }
      underlying.flush()
    }

    override def close(): Unit = { flush(); underlying.close() }
  }
}
*/