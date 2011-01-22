package com.bubblefoundry.bfurbanairship

import dispatch._
import dispatch.liftjson.Js._
import net.liftweb.json.JsonAST._
import net.liftweb.json.Serialization.write

import net.liftweb.common.Box
import net.liftweb.util.Helpers


object LiftJsonHelpers {
  implicit val formats = new net.liftweb.json.DefaultFormats {
    override def dateFormatter = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
  }
}

case class Quiettime(start: String, end: String)
case class Device(device_token: String, alias: Option[String], tags: Option[List[String]], badge: Option[Int], quiettime: Option[Quiettime], tz: Option[String], last_registration: Option[java.util.Date])
object Device {
  def apply(device_token: String): Device = Device(device_token, None, None, None, None, None, None)
  def apply(device_token: String, alias: String): Device = Device(device_token, Some(alias), None, None, None, None, None)
  def apply(device_token: String, alias: Option[String], tags: Option[List[String]], badge: Option[Int], quiettime: Option[Quiettime], tz: Option[String]): Device = Device(device_token, alias, tags, badge, quiettime, tz, None)
}

abstract trait APS
object APS {
  def apply(alert: String): APSString = APSString(alert, None, None)
  def apply(alert: Map[String, Any]): APSDict = APSDict(alert, None, None)
}


case class APSString(alert: String, badge: Option[Int], sound: Option[String]) extends APS
object APSString {
  def apply(alert: String, badge: Int, sound: String): APSString = APSString(alert, Some(badge), Some(sound))
}

case class APSDict(alert: Map[String, Any], badge: Option[Int], sound: Option[String]) extends APS
object APSDict {
  def apply(alert: Map[String, Any], badge: Int, sound: String): APSDict = APSDict(alert, Some(badge), Some(sound))
}

case class PushMessage[T <: APS](device_tokens: Option[List[String]], aliases: Option[List[String]], tags: Option[List[String]], schedule_for: Option[List[java.util.Date]], exclude_tokens: Option[List[String]], aps: Option[T])
object PushMessage {
  def apply[T <: APS](device_tokens: List[String], aliases: List[String], tags: List[String], schedule_for: List[java.util.Date], exclude_tokens: List[String], aps: T): PushMessage[T] = PushMessage(Some(device_tokens), Some(aliases), Some(tags), Some(schedule_for), Some(exclude_tokens), Some(aps))
  def apply[T <: APS](device_tokens: List[String], schedule_for: List[java.util.Date], aps: T): PushMessage[T] = PushMessage(Some(device_tokens), None, None, Some(schedule_for), None, Some(aps))
}

case class HourlyStatistics(start: java.util.Date, messages: Int, android_messages: Int, bb_messages: Int)

class UrbanAirship(app_token: String, app_secret: Box[String], app_master_secret: Box[String]) {  
  lazy val http = new Http
  lazy val urbanairshipReq = :/("go.urbanairship.com").secure
  lazy val apiReq = urbanairshipReq / "api"
  lazy val pushReq = apiReq / "push"

  lazy val registerReq = apiReq / "device_tokens"
  lazy val statisticsReq = pushReq / "stats"
  
  def this(app_token: String, app_secret: Option[String], app_master_secret: Option[String]) = {
    this(app_token, Box(app_secret), Box(app_master_secret))
  }

  def register_device(device: Device): Box[Device] = app_secret.flatMap(secret => {
    import LiftJsonHelpers._
    val req = registerReq / device.device_token <<< write(device) <:< Map("Content-Type" -> "application/json") as (app_token, secret)
    Helpers.tryo(http(req ># (json => {
      json.extract[Device]
    })))
  }) ?~ "App Secret required"
  
  def push[T <: APS](message: PushMessage[T]): Box[String] = app_master_secret.flatMap(secret => {
    import LiftJsonHelpers._
    val req = pushReq / "" << write(message) <:< Map("Content-Type" -> "application/json") as (app_token, secret)
    Helpers.tryo(http(req as_str))
  }) ?~ "App Master Secret Required"
  
  def statistics(start: String, end: String): Box[List[HourlyStatistics]] = app_master_secret.flatMap(secret => {
    val req = statisticsReq <<? Map("start" -> start, "end" -> end) as (app_token, secret) 
    Helpers.tryo(http(req ># (json => {
      import LiftJsonHelpers._
      json.children.map(_.extract[HourlyStatistics])
    })))
  }) ?~ "App Master Secret required"
     
}