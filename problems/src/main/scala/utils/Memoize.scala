package utils

import scala.annotation.{experimental, MacroAnnotation}
import scala.collection.concurrent.TrieMap
import scala.quoted.*

/** Simple in-memory memoization.
 *
 * `Memoize.caching(f)` returns a memoized version of `f`. Each call to
 * `caching` allocates its own cache, so distinct memoized functions never share
 * entries — assign the result to a `val` once and call that.
 *
 * Arguments are keyed by an `AnyRef` derived via a [[Memoize.Key]] type-class.
 * Unlike `toString`-based keying, the key is a real object whose
 * `equals`/`hashCode` decide cache identity — lossless, and you control, per
 * type, what actually matters. Give an argument a trivial (constant) `Key` to
 * exclude it from the cache identity.
 *
 * The returned function is safe for concurrent use (the body may run more than
 * once under a race, but only one result is retained). Caches never evict.
 */
object Memoize {
  def caching[A: Key, R](f: A => R): A => R = {
    val cache = TrieMap.empty[AnyRef, Any]
    a => lookup(cache, Key[A].key(a))(f(a))
  }

  def caching[A: Key, B: Key, R](f: (A, B) => R): (A, B) => R = {
    val cache = TrieMap.empty[AnyRef, Any]
    (a, b) => lookup(cache, (Key[A].key(a), Key[B].key(b)))(f(a, b))
  }

  def caching[A: Key, B: Key, C: Key, R](f: (A, B, C) => R): (A, B, C) => R = {
    val cache = TrieMap.empty[AnyRef, Any]
    (a, b, c) => lookup(cache, (Key[A].key(a), Key[B].key(b), Key[C].key(c)))(f(a, b, c))
  }

  def caching[A: Key, B: Key, C: Key, D: Key, R](f: (A, B, C, D) => R): (A, B, C, D) => R = {
    val cache = TrieMap.empty[AnyRef, Any]
    (a, b, c, d) =>
      lookup(cache, (Key[A].key(a), Key[B].key(b), Key[C].key(c), Key[D].key(d)))(f(a, b, c, d))
  }

  private def lookup[R](cache: TrieMap[AnyRef, Any], key: AnyRef)(body: => R): R =
    cache.getOrElseUpdate(key, body).asInstanceOf[R]

  /** Derives a cache key for values of type `A`. The returned `AnyRef`'s
   * `equals`/`hashCode` define when two `A`s share a cache entry. */
  trait Key[A] {
    def key(value: A): AnyRef
  }

  object Key {
    def apply[A](using k: Key[A]): Key[A] = k

    /** Key from an arbitrary extraction function. */
    def by[A, B <: AnyRef](f: A => B): Key[A] = (value: A) => f(value)

    /** All values of `A` collapse to one entry — i.e. the argument is ignored
     * for caching. Useful for contextual params like `ExceptionContext`. */
    def constant[A]: Key[A] = _ => Constant
    private object Constant

    /** Boxed value; correct for value classes and case classes (structural
     * equality). NOT safe for reference types with identity `equals`. */
    given Key[Int]     = _.asInstanceOf[AnyRef]
    given Key[Long]    = _.asInstanceOf[AnyRef]
    given Key[Boolean] = _.asInstanceOf[AnyRef]
    given Key[Char]    = _.asInstanceOf[AnyRef]
    given Key[String]  = identity(_)
  }
}

/** Memoize a method on its value parameters, backed by a per-instance cache.
 *
 * `@memoized def f(x: X)(using c: C): R = body` is rewritten to
 * {{{
 *   private lazy val f$memoCache = TrieMap.empty[AnyRef, Any]
 *   def f(x: X)(using c: C): R =
 *     val k = List(Key[X].key(x), Key[C].key(c))
 *     f$memoCache.get(k) match
 *       case Some(v) => v.asInstanceOf[R]
 *       case _ => val res = body; f$memoCache.update(k, res); res
 * }}}
 * The cache is an ordinary field, so each instance has its own (and it is
 * collected with the instance — no global retention). A [[Memoize.Key]] given
 * must be in scope for every parameter type; use [[Memoize.Key.constant]] to
 * exclude one (e.g. an `ExceptionContext`). (The field is `lazy` to work around
 * a compiler crash in `ParamForwarding` when adding a strict field to a class
 * that has constructor parameters.)
 *
 * If `R` is a `Future[_]`, the *in-flight* future is cached (published before it
 * completes) so concurrent identical calls share one computation rather than each
 * starting its own; a future that completes with a failure is evicted, so failures
 * are never cached and the next call retries.
 *
 * `MacroAnnotation` is experimental: the annotated definition (or its enclosing
 * scope) must be under experimental mode — mark it `@experimental` or compile
 * with `-experimental`.
 */
@experimental
class memoized extends MacroAnnotation {
  def transform(using Quotes)(
      tree: quotes.reflect.Definition,
      companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] = {
    import quotes.reflect.*

    tree match {
      case DefDef(name, paramss, tpt, Some(rhs)) =>
        val defSym = tree.symbol
        val params = paramss.flatMap(_.params).collect { case v: ValDef => v }

        // Per-instance (or per-object) cache field, sibling to the method.
        val cacheTpe = TypeRepr.of[TrieMap[AnyRef, Any]]
        val cacheSym = Symbol.newVal(
          Symbol.spliceOwner, Symbol.freshName(name + "$memoCache"),
          cacheTpe, Flags.Private | Flags.Lazy, Symbol.noSymbol)
        val cacheVal = ValDef(cacheSym, Some('{ TrieMap.empty[AnyRef, Any] }.asTerm))
        val cacheRef = Ref(cacheSym).asExprOf[TrieMap[AnyRef, Any]]

        val keyParts: List[Expr[AnyRef]] = params.map { v =>
          v.tpt.tpe.asType match
            case '[t] =>
              Expr.summon[Memoize.Key[t]] match
                case Some(k) => '{ $k.key(${ Ref(v.symbol).asExprOf[t] }) }
                case None =>
                  report.errorAndAbort(
                    s"No Memoize.Key given for parameter ${v.name}: ${v.tpt.tpe.show}", v.pos)
        }
        val keyExpr = Expr.ofList(keyParts)

        // A `Future`-returning method is memoized on the *in-flight* future, not its resolved value:
        // the future is published to the cache before it completes, so concurrent identical calls
        // share one computation instead of each starting its own (e.g. a feedback poll's concurrent
        // fan-out all parsing the same input). A failed future is evicted once it completes, so
        // failures are never cached — the next call retries.
        val futureSym = Symbol.requiredClass("scala.concurrent.Future")
        val isFuture = tpt.tpe.baseClasses.contains(futureSym)

        // Inline the lookup rather than calling a helper with a by-name `body`: a by-name
        // argument becomes a closure, and lifting the body's captures out of that closure
        // trips a compiler bug (LambdaLift "could not find proxy") when the body references
        // private members of the enclosing object. Keeping the body directly in the method
        // avoids the extra closure entirely.
        //
        // Re-own the whole new body to the (copied) method symbol; the constructed term is
        // otherwise owned by the quote's synthetic owner, so references to the method's
        // params/locals cannot be resolved later ("could not find proxy").
        val newRhs =
          if isFuture then
            tpt.tpe.baseType(futureSym).typeArgs.head.asType match
              case '[t] =>
                val body = rhs.asExprOf[scala.concurrent.Future[t]]
                '{
                  val c = $cacheRef
                  val k: AnyRef = $keyExpr
                  c.get(k) match
                    case Some(v) => v.asInstanceOf[scala.concurrent.Future[t]]
                    case _ =>
                      // Publish an incomplete promise atomically so only the winner runs `body`.
                      val promise = scala.concurrent.Promise[t]()
                      c.putIfAbsent(k, promise.future) match
                        case Some(existing) => existing.asInstanceOf[scala.concurrent.Future[t]]
                        case None =>
                          val work: scala.concurrent.Future[t] =
                            try $body
                            catch case scala.util.control.NonFatal(e) => scala.concurrent.Future.failed[t](e)
                          work.onComplete { result =>
                            // Don't cache failures: drop the entry so the next call re-computes.
                            if (result.isFailure) c.remove(k, promise.future)
                            promise.complete(result)
                          }(scala.concurrent.ExecutionContext.parasitic)
                          promise.future
                }.asTerm.changeOwner(defSym)
          else
            tpt.tpe.asType match
              case '[r] =>
                val body = rhs.asExprOf[r]
                '{
                  val c = $cacheRef
                  val k: AnyRef = $keyExpr
                  c.get(k) match
                    case Some(v) => v.asInstanceOf[r]
                    case _ =>
                      val res: r = $body
                      c.update(k, res)
                      res
                }.asTerm.changeOwner(defSym)

        val newDef = DefDef.copy(tree)(name, paramss, tpt, Some(newRhs))
        List(cacheVal, newDef)

      case _ =>
        report.errorAndAbort("@memoized is only allowed on a concrete method (def with a body)", tree.pos)
    }
  }
}
