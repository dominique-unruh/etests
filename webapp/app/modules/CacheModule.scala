package modules

// app/modules/CacheModule.scala
import com.google.inject.AbstractModule
import com.typesafe.scalalogging.Logger
import play.api.inject.ApplicationLifecycle
import utils.Cache

import javax.inject.*
import scala.concurrent.Future

class CacheModule extends AbstractModule {
  override def configure(): Unit = {
    bind(classOf[CacheInitializer]).asEagerSingleton()
  }
}

@Singleton
class CacheInitializer @Inject()(lifecycle: ApplicationLifecycle) {
  // Register cleanup
  lifecycle.addStopHook { () =>
    logger.debug("CacheInitializer: closing Cache")
    // Ensure Cache gets closed to avoid locks when Play reloads classes in development mode
    Future.successful(Cache.close())
  }

  private val logger = Logger[CacheInitializer]
}
