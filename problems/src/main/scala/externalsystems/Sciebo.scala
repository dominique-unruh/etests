package externalsystems

import com.typesafe.scalalogging.Logger
import org.aarboard.nextcloud.api.exception.NextcloudApiException
import org.aarboard.nextcloud.api.{AuthenticationConfig, NextcloudConnector}
import org.aarboard.nextcloud.api.filesharing.{Share, SharePermissions, ShareType}
import utils.{PersistentCache, Utils}

import java.io.IOException
import java.nio.file.Path
import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.jdk.CollectionConverters.CollectionHasAsScala

object Sciebo {
  private val minSecondsBetweenRequests = 5
  private var secondsBetweenRequests: Double = minSecondsBetweenRequests

  private lazy val username: String = Utils.getSystemProperty("sciebo.username", "your Sciebo username")
  private lazy val password: String =
    Utils.readOrPromptKeyring("etests", "sciebo.password", "your Sciebo app password") {
      Utils.promptForString(s"Sciebo app password for $username")
    }
  private lazy val client: NextcloudConnector =
    new NextcloudConnector("https://rwth-aachen.sciebo.de", username, password)

  private var lastRequest = Instant.MIN
  def getPublicReadLink(root: Path, path: Path): String = {
    def once(): String = {
      val subpath = root.relativize(path).toString
      PersistentCache.getOrCompute[String](s"SCIEBO-PUBLIC-LINK2:$subpath".getBytes, _.getBytes, new String(_)) {
        synchronized {
          Utils.waitUntil(lastRequest.plus((secondsBetweenRequests * 1000).toLong, ChronoUnit.MILLIS))
          lastRequest = Instant.now()
          logger.info(s"Requesting Sciebo link for $subpath")
          val permissions = SharePermissions(SharePermissions.SingleRight.READ, SharePermissions.SingleRight.SHARE)
          val existing = client.getShares(subpath, false, false).asScala.find(share =>
            share.getSharePermissions.getCurrentPermission == permissions.getCurrentPermission &&
              share.getShareType == ShareType.PUBLIC_LINK)
          logger.debug(s"Existing link: $existing")
          val share = existing.getOrElse {
            Thread.sleep((secondsBetweenRequests*1000).toLong)
            lastRequest = Instant.now()
            client.doShare(subpath, ShareType.PUBLIC_LINK, null, false, null, permissions) }
          logger.info(s"Link to $subpath: ${share.getUrl}${if (existing.nonEmpty) " (already existed)" else ""}")
          lastRequest = Instant.now()
          share.getUrl
        }
      }
    }
    while (true) {
      try {
        val result = once()
        secondsBetweenRequests *= 0.99
        if (secondsBetweenRequests < minSecondsBetweenRequests)
          secondsBetweenRequests = minSecondsBetweenRequests
        return result
      } catch {
        case e: NextcloudApiException =>
          e.printStackTrace()
          secondsBetweenRequests *= 2
          logger.warn(s"Retrying with delay $secondsBetweenRequests")
      }
    }
    throw AssertionError("unreachable code") // Unreachable
  }

  private val logger = Logger[Sciebo.type]
}
