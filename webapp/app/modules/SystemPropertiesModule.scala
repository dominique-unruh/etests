package modules

import com.google.inject.AbstractModule
import utils.Utils

/** Module to initialize the system properties from java.properties (if it exists). */
class SystemPropertiesModule extends AbstractModule {
  override def configure(): Unit = {
    Utils.loadSystemProperties()
  }
}