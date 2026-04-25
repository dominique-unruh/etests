package externalsystems

import com.typesafe.scalalogging.Logger
import org.aarboard.nextcloud.api.{AuthenticationConfig, NextcloudConnector}
import org.aarboard.nextcloud.api.filesharing.{Share, SharePermissions, ShareType}
import utils.{PersistentCache, Utils}

import java.nio.file.Path
import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.jdk.CollectionConverters.CollectionHasAsScala

object Sciebo {
  val secondsBetweenRequests = 20

  private lazy val username: String = Utils.getSystemProperty("sciebo.username", "your Sciebo username")
  private def password: String = Utils.getSystemProperty("sciebo.password", "your Sciebo app password")
  private lazy val sharePath: Path = {
    val path = Utils.getSystemPropertyPath("sciebo.sharepath", "path with published grading reports, relative to your Sciebo folder")
    assert(!path.isAbsolute)
    path
  }
  private lazy val client: NextcloudConnector = {
//    val password = Utils.askPassword("Sciebo password")
    new NextcloudConnector("https://rwth-aachen.sciebo.de", username, password)
  }

  private var lastRequest = Instant.MIN
  def getPublicReadLink(path: String): String =
    PersistentCache.getOrCompute[String](s"SCIEBO-PUBLIC-LINK2:$path".getBytes, _.getBytes, new String(_)) {
    synchronized {
      Utils.waitUntil(lastRequest.plus(secondsBetweenRequests, ChronoUnit.SECONDS))
      logger.info(s"Requesting Sciebo link for $path")
      val permissions = SharePermissions(SharePermissions.SingleRight.READ, SharePermissions.SingleRight.SHARE)
      val existing = client.getShares(path, false, false).asScala.find(share =>
        share.getSharePermissions.getCurrentPermission == permissions.getCurrentPermission &&
          share.getShareType == ShareType.PUBLIC_LINK)
      logger.debug(s"Existing link: $existing")
      val share = existing.getOrElse(
        // TODO Currently passwords are enforced. Replace password by null later.
        client.doShare(path, ShareType.PUBLIC_LINK, null, false, "password", permissions))
      logger.info(s"Link to $path: ${share.getUrl}${if (existing.nonEmpty) " (already existed)" else ""}")
      lastRequest = Instant.now()
      share.getUrl
      }
  }

  private val logger = Logger[Sciebo.type]
}
