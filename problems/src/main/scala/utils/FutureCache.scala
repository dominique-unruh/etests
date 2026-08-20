package utils

import assessments.MathContext.FunctionResult.Success
import com.github.blemale.scaffeine.{AsyncLoadingCache, Scaffeine}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.util.Try

object FutureCache {
  private val cache: AsyncLoadingCache[Any, Try[Any]] =
    Scaffeine()
      .maximumSize(10_000)
      .expireAfterWrite(1.hour)
      .buildAsyncFuture { key =>
        Future.failed(new IllegalStateException(
          s"No loader registered for key=$key; use evaluate(key)(body)"
        ))
      }


  def evaluate[A](key: Any)(body: => A): Future[A] = {
    val future = cache.get(key, _ => Try(body))

    future.asInstanceOf[Future[Try[A]]].transform(_.flatten)
  }

  /** Like [[evaluate]], but if a cached value is found and `guard` returns true for it,
   * the cached value is discarded, the entry recomputed via `body`, and the new value cached. */
  def evaluateGuarded[A](key: Any)(guard: A => Boolean)(body: => A): Future[A] =
    evaluate(key)(body).flatMap { value =>
      if (guard(value)) {
        cache.synchronous().invalidate(key)
        evaluate(key)(body)
      } else
        Future.successful(value)
    }

  def evaluateFuture[A](key: Any)(body: => Future[A]): Future[A] = {
    val future = cache.getFuture(key, _ => body.transform(scala.util.Success.apply))

    future.asInstanceOf[Future[Try[A]]].transform(_.flatten)
  }

  def clear(): Unit =
    cache.synchronous().invalidateAll()
}