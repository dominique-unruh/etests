package externalsystems

import org.aarboard.nextcloud.api.{AuthenticationConfig, NextcloudConnector}
import org.aarboard.nextcloud.api.filesharing.{Share, SharePermissions, ShareType}
import utils.Utils

import java.nio.file.Path
import scala.jdk.CollectionConverters.CollectionHasAsScala

object Sciebo {
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

  def getPublicReadLink(path: String): String = {
    val permissions = SharePermissions(SharePermissions.SingleRight.READ, SharePermissions.SingleRight.SHARE)
    val existing = client.getShares(path, false, false).asScala.find(share =>
      share.getSharePermissions.getCurrentPermission == permissions.getCurrentPermission &&
      share.getShareType == ShareType.PUBLIC_LINK)
    val share = existing.getOrElse(
        client.doShare(path, ShareType.PUBLIC_LINK, null, false, null, permissions))
    share.getUrl
  }
}
