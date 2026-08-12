import sbt._
import Keys._

object TargetIdPlugin extends AutoPlugin {
  override def trigger = allRequirements

  val targetId = sys.env.getOrElse("SBT_TARGET_ID", java.net.InetAddress.getLocalHost.getHostName)

  val targetIdSettings: Seq[Setting[_]] = Seq(
    target := (ThisBuild / baseDirectory).value / "target" / targetId / thisProject.value.id
  )

  override def projectSettings: Seq[Setting[_]] = targetIdSettings
}

object TargetIdPlayOverride extends AutoPlugin {
  override def requires = play.sbt.PlayScala
  override def trigger = allRequirements
  override def projectSettings = TargetIdPlugin.targetIdSettings
}

object TargetIdWebOverride extends AutoPlugin {
  override def requires = com.typesafe.sbt.web.SbtWeb
  override def trigger = allRequirements
  override def projectSettings = TargetIdPlugin.targetIdSettings
}
