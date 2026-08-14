package assessments

import utest.Tests
import utest.framework.{TestCallTree, Tree}

/** A single name-labelled test tree — the combined view of utest's two parallel structures
 * ([[utest.framework.Tree]] of names + [[utest.framework.TestCallTree]] of bodies), i.e. roughly a
 * `Tree[(name, runner)]`.
 *
 *   - A node with no `children` is a leaf: `run` is the test body, executed when the leaf runs.
 *   - A node with `children` is a group: `run` is setup, executed (each time) on the path down to a
 *     descendant leaf, then the children are entered.
 *
 * Convert to a utest suite with [[toTests]]. (It cannot itself `extend Tests`: `Tests` is a case
 * class, and a case class may not be extended by another case class.) */
final class Test(val name: String, val run: ExceptionContext ?=> Unit, val children: Seq[Test] = Seq.empty) {
  assert(name != null)
  assert(name != "")
  private def nameTree: Tree[String] =
    Tree(name, children.map(_.nameTree) *)

  private def callTree(using exceptionContext: ExceptionContext): TestCallTree =
    if (children.isEmpty)
      new TestCallTree(Left(run)) // leaf: forcing runs the body
    else
      new TestCallTree(Right { run; children.map(_.callTree).toIndexedSeq }) // group: setup, then descend

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
    run
    children.foreach(_.runAll())
  }

  def appendChild(test: Test): Test = new Test(name, run, children.appended(test))
  def withName(name: String): Test = new Test(name, run, children)
}

object Test {
  def apply(name: String, run: ExceptionContext ?=> Unit, children: Seq[Test] = Seq.empty): Test =
    new Test(name, run, children)
}
