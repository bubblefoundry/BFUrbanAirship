import sbt._

class BFUrbanAirshipProject(info: ProjectInfo) extends DefaultProject(info)
{
  // val dispatch_json = "net.databinder" %% "dispatch-json" % "0.7.8"
  // val dispatch_lift_json = "net.databinder" %% "dispatch-lift-json" % "0.7.8"
  val scalatoolsSnapshot = "Scala Tools Snapshot" at "http://scala-tools.org/repo-snapshots/"
  val liftVersion = "2.3-M1"
  
  val dispatch_vers = "0.8.0.Beta3"
  // lazy val dispatch_meetup = "net.databinder" %% "dispatch-meetup" % dispatch_vers
  lazy val dispatch_gae = "net.databinder" %% "dispatch-http-gae" % dispatch_vers
  lazy val dispatch_liftjson = "net.databinder" %% "dispatch-lift-json" % dispatch_vers

  val appengineRepo = "nexus" at "http://maven-gae-plugin.googlecode.com/svn/repository/"
  
  // val dispatch = "net.databinder" %% "dispatch" % "0.7.8"
  val lift_common = "net.liftweb" %% "lift-common" % liftVersion
  val lift_util = "net.liftweb" %% "lift-util" % liftVersion
}
