import sbt._

class BFUrbanAirshipProject(info: ProjectInfo) extends DefaultProject(info)
{
  val dispatch = "net.databinder" %% "dispatch-lift-json" % "0.7.8"
}
