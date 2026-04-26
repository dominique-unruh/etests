package modules

import com.typesafe.scalalogging.Logger
import modules.CacheInvalidationModule.CacheReloadHook
import play.api.{Configuration, Environment}
import play.api.inject.{ApplicationLifecycle, Binding, Module as PlayModule}
import utils.FutureCache

import javax.inject.*
import scala.concurrent.{ExecutionContext, Future}

class CacheInvalidationModule extends PlayModule {
  override def bindings(environment: Environment, configuration: Configuration): Seq[Binding[_]] =
    Seq(
      bind[CacheInvalidationModule.CacheReloadHook].toSelf.eagerly()
    )
}

object CacheInvalidationModule {
  @Singleton
  final class CacheReloadHook @Inject()(lifecycle: ApplicationLifecycle) {
    lifecycle.addStopHook { () => Future.successful {
      logger.debug("Clearing cache because of reload.")
      FutureCache.clear()
    }}
  }

  private val logger = Logger[CacheInvalidationModule]
}
