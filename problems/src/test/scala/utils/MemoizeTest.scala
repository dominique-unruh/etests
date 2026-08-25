package utils

import org.scalatest.funsuite.AnyFunSuiteLike

import java.util.concurrent.atomic.AtomicInteger

class MemoizeTest extends AnyFunSuiteLike {

  // A parameter type whose identity must NOT affect caching.
  class Ctx
  given Memoize.Key[Ctx] = Memoize.Key.constant

  object Svc {
    val calls = new AtomicInteger(0)
    @memoized def f(x: Int, ctx: Ctx): Int = {
      calls.incrementAndGet()
      x + 1
    }
  }

  test("@memoized caches on relevant params, ignores constant-keyed") {
    assert(Svc.f(1, new Ctx) == 2)
    assert(Svc.f(1, new Ctx) == 2) // different ctx, cache hit
    assert(Svc.calls.get() == 1)
    assert(Svc.f(2, new Ctx) == 3)
    assert(Svc.calls.get() == 2)
  }

  // Mirrors StackParser: an object method whose body has local vals and captures a private
  // member. Guards the LambdaLift "could not find proxy" crashes fixed via changeOwner.
  object Captures {
    private val prefix = "p:"
    @memoized def f(x: String, ctx: Ctx): String = {
      val local = x.reverse
      prefix + local
    }
  }

  test("@memoized body may use locals and capture private members") {
    assert(Captures.f("ab", new Ctx) == "p:ba")
    assert(Captures.f("ab", new Ctx) == "p:ba")
  }

  class ParamCls(base: Int) {
    @memoized def f(x: Int): Int = base + x
  }

  test("@memoized on a class method with constructor param, per-instance") {
    val a = new ParamCls(100)
    val b = new ParamCls(200)
    assert(a.f(1) == 101)
    assert(a.f(1) == 101)
    assert(b.f(1) == 201) // separate instance cache
  }

  test("combinator caching allocates a fresh cache per call") {
    val calls = new AtomicInteger(0)
    val memoized = Memoize.caching[Int, Int] { x => calls.incrementAndGet(); x + 1 }
    assert(memoized(5) == 6)
    assert(memoized(5) == 6)
    assert(calls.get() == 1)
  }
}
