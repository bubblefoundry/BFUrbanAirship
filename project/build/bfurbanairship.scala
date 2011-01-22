import sbt._

class BFUrbanAirshipProject(info: ProjectInfo) extends DefaultProject(info)
{
  // val dispatch_json = "net.databinder" %% "dispatch-json" % "0.7.8"
  // val dispatch_lift_json = "net.databinder" %% "dispatch-lift-json" % "0.7.8"
  val dispatch = "net.databinder" %% "dispatch" % "0.7.8"
  val lift_common = "net.liftweb" %% "lift-common" % "2.2"
  val lift_util = "net.liftweb" %% "lift-util" % "2.2"
}
