package controllers

import com.typesafe.scalalogging.Logger
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents, Request}

import javax.inject.{Inject, Singleton}

// TODO remove (doesn't work)
@Singleton
class BrowserController @Inject()(val controllerComponents: ControllerComponents) extends BaseController {
  def browse(path: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    import scala.reflect.runtime.universe.runtimeMirror
    val mirror = runtimeMirror(this.getClass.getClassLoader)
//    val packageSymbol = mirror.staticPackage("scala.collection.mutable")
    logger.debug(path.stripSuffix("/").replace('/', '.'))
    val packageSymbol = mirror.staticPackage(path.stripSuffix("/").replace('/', '.'))
    logger.debug(packageSymbol.toString)
    val members = packageSymbol.info.members.toList.filter(_.isPublic)
    logger.debug(members.toString())

    val result = members.mkString("\n")
    Ok(result)
  }

  private val logger = Logger[BrowserController]
}
