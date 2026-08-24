package utils

import assessments.MathContext.FunctionResult.Success
import com.github.blemale.scaffeine.{AsyncLoadingCache, Scaffeine}
import com.typesafe.scalalogging.Logger

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

  def invalidate(key: Any): Unit =
    cache.synchronous().invalidate(key)

  def evaluate[A](key: Any)(body: => A): Future[A] = {
    val future = cache.get(key, _ => Try(body))

    future.asInstanceOf[Future[Try[A]]].transform(_.flatten)
  }

  def evaluateGuarded[A](key: Any, cacheGuard: A => Boolean)(body: => A): Future[A] = {
    val future = evaluate(key)(body)
    future.value match {
      case Some(util.Success(value)) if cacheGuard(value) =>
        logger.debug("Invalidating cache key (fails guard)")
        invalidate(key)
        evaluate(key)(body)
      case None =>
        future
    }
  }

  def evaluateFuture[A](key: Any)(body: => Future[A]): Future[A] = {
    val future = cache.getFuture(key, _ => body.transform(scala.util.Success.apply))

    future.asInstanceOf[Future[Try[A]]].transform(_.flatten)
  }

  def clear(): Unit =
    cache.synchronous().invalidateAll()

  private val logger = Logger[this.type]
}