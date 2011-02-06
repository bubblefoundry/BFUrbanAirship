import sbt._

class BFUrbanAirshipProject(info: ProjectInfo) extends DefaultProject(info)
{
  // val dispatch_json = "net.databinder" %% "dispatch-json" % "0.7.8"
  // val dispatch_lift_json = "net.databinder" %% "dispatch-lift-json" % "0.7.8"
  val scalatoolsSnapshot = "Scala Tools Snapshot" at "http://scala-tools.org/repo-snapshots/"
  val liftVersion = "2.3-SNAPSHOT"
  
  val dispatch = "net.databinder" %% "dispatch" % "0.7.8"
  val lift_common = "net.liftweb" %% "lift-common" % liftVersion
  val lift_util = "net.liftweb" %% "lift-util" % liftVersion
}
