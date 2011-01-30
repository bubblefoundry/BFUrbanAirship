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
case class Device(device_token: String, alias: Option[String], tags: Option[List[String]], badge: Option[Int], quiettime: Option[Quiettime], tz: Option[String], last_registration: Option[java.util.Date], active: Option[Boolean])
object Device {
  def apply(device_token: String): Device = Device(device_token, None, None, None, None, None, None, None)
  def apply(device_token: String, alias: String): Device = Device(device_token, Some(alias), None, None, None, None, None, None)
  def apply(device_token: String, alias: Option[String], tags: Option[List[String]], badge: Option[Int], quiettime: Option[Quiettime], tz: Option[String]): Device = Device(device_token, alias, tags, badge, quiettime, tz, None, None)
}

case class DevicesPage(device_tokens_count: Int, device_tokens: List[Device], current_page: Int, num_pages: Int, active_device_tokens_count: Int)

case class DevicesCount(device_tokens_count: Int, active_device_tokens_count: Int)

abstract trait APS
object APS {
  def apply(alert: String): APSString = APSString(alert, None, None)
  def apply(alert: Map[String, Any]): APSDict = APSDict(alert, None, None)
}
// the badge string can be: "auto", "+1", "-1", or any valid Int. Its absence removes any existing badge.
case class APSString(alert: String, badge: Option[String], sound: Option[String]) extends APS
object APSString {
  def apply(alert: String, badge: String, sound: String): APSString = APSString(alert, Some(badge), Some(sound))
}
case class APSDict(alert: Map[String, Any], badge: Option[String], sound: Option[String]) extends APS
object APSDict {
  def apply(alert: Map[String, Any], badge: String, sound: String): APSDict = APSDict(alert, Some(badge), Some(sound))
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

  lazy val devicesReq = apiReq / "device_tokens"
  lazy val statisticsReq = pushReq / "stats"
  
  def this(app_token: String, app_secret: Option[String], app_master_secret: Option[String]) = {
    this(app_token, Box(app_secret), Box(app_master_secret))
  }

  def register_device(device: Device): Box[Device] = app_secret.flatMap(secret => {
    import LiftJsonHelpers._
    val req = devicesReq / device.device_token <<< write(device) <:< Map("Content-Type" -> "application/json") as (app_token, secret)
    Helpers.tryo(http(req ># (json => {
      json.extract[Device]
    })))
  }) ?~ "App Secret required"
  
  // GET device 
  
  // get device tokens
  def devices: Box[Iterator[Device]] = app_master_secret.flatMap(secret => {
    import LiftJsonHelpers._
    
    def getPage(page: Int) = {
      val req = devicesReq / "" <<? Map("page" -> page) as (app_token, secret)
      Helpers.tryo(http(req ># (json => {
        json.extract[DevicesPage]
      })))
    }
    
    // get the first page
    val page1 = getPage(1)
    // within its box
    page1.map(page => {
      // turn the first page's List[Device] into an Iterator[Device]
      page.device_tokens.toIterator ++ ({
          // then for any additional pages (Iterator.range(2, 1) == Iterator.empty)
          // take the page and be ready to reduce down our inner Iterator
          Iterator.range(2, page.num_pages).flatMap(p => {
            // get the page, then just its List[Device], escape the Box, to an Iterator
            getPage(p).map(_.device_tokens).getOrElse(Nil).toIterator
          })
        })
    })
  }) ?~ "App Master Secret Required"
  
  def devices_count: Box[DevicesCount] = app_master_secret.flatMap(secret => {
    import LiftJsonHelpers._
    val req = devicesReq / "count" / "" as (app_token, secret)
    Helpers.tryo(http(req ># (_.extract[DevicesCount])))
  }) ?~ "App Master Secret Required"
  
  def push[T <: APS](message: PushMessage[T]): Box[String] = app_master_secret.flatMap(secret => {
    import LiftJsonHelpers._
    val req = pushReq / "" << write(message) <:< Map("Content-Type" -> "application/json") as (app_token, secret)
    Helpers.tryo(http(req as_str))
  }) ?~ "App Master Secret Required"
  
  /*
  [
    {
        "device_tokens": [
            "some_device_token",
            "another_device_token"
        ],
        "aps": {
             "badge": 15,
             "alert": "Hello from Urban Airship!",
             "sound": "cat.caf"
        }
    },
    {
        "device_tokens": [
            "yet_another_device_token"
        ],
        "aliases": [
            "some_alias",
            "another_alias"
        ],
        "aps": {
            "badge": 12
        }
    }
  ]
  */
  def pushMany[T <: APS](messages: List[PushMessage[T]]): Box[String] = app_master_secret.flatMap(secret => {
    import LiftJsonHelpers._
    val req = pushReq / "batch" / "" << write(messages) <:< Map("Content-Type" -> "application/json") as (app_token, secret)
    Helpers.tryo(http(req as_str))
  }) ?~ "App Master Secret Required"
  
  // schedule_for should be a string here, not a list of strings? Though UA seems to allow a string in places where you'd otherwise have a list of one.
  def pushAll[T <: APS](message: PushMessage[T]): Box[String] = app_master_secret.flatMap(secret => {
    import LiftJsonHelpers._
    val req = pushReq / "broadcast" / "" << write(message) <:< Map("Content-Type" -> "application/json") as (app_token, secret)
    Helpers.tryo(http(req as_str))
  }) ?~ "App Master Secret Required"
  
  // delete scheduled with POST to https://go.urbanairship.com/api/push/scheduled/
  /*
  {
    "cancel": [
        "https://go.urbanairship.com/api/push/scheduled/XX",
        "https://go.urbanairship.com/api/push/scheduled/XY"
    ],
    "cancel_aliases": [
        "some_alias",
        "another_alias"
    ],
    "cancel_device_tokens": [
        "example_device_token",
        "other_example_device_token"
    ]
  }
  */
  
  // updated scheduled with https://go.urbanairship.com/api/push/scheduled/alias/<your alias>
  /*
  {
    "alias": "some_alias",
    "schedule_for": "2010-06-05 22:15:00",
    "payload": {
        "device_tokens": ["some_device_token"],
        "aps": {
            "alert": "Hello from the past!",
            "sound": "cat.caf"
        }
    }
  }
  */
  
  // feedback service
  // /api/device_tokens/feedback/?since=<timestamp>, where timestamp is ISO 8601 (e.g. 2009-06-01+13:00:00)
  /*
  [
    {
       "device_token": "1234123412341234123412341234123412341234123412341234123412341234",
       "marked_inactive_on": "2009-06-22 10:05:00",
       "alias": "bob"
    },
    {
       "device_token": "ABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCD",
       "marked_inactive_on": "2009-06-22 10:07:00",
       "alias": null
    }
  ]
  */
    
/*
  def pushAll[T <: APS](message: PushMessage[T]): Box[String] = (for (
    secret <- app_master_secret ?~! "App Master Secret Required"
    message <- Full(message) ?~! ""
  ) yield {
    Helpers.tryo(http(req as_str))
  }).flatMap(resp => resp)
  
  app_master_secret.flatMap(secret => {
    import LiftJsonHelpers._
    val req = pushReq / "broadcast" / "" << write(message) <:< Map("Content-Type" -> "application/json") as (app_token, secret)
    Helpers.tryo(http(req as_str))
  }) ?~ "App Master Secret Required"
*/

  def statistics(start: String, end: String): Box[List[HourlyStatistics]] = app_master_secret.flatMap(secret => {
    val req = statisticsReq <<? Map("start" -> start, "end" -> end) as (app_token, secret) 
    Helpers.tryo(http(req ># (json => {
      import LiftJsonHelpers._
      json.children.map(_.extract[HourlyStatistics])
    })))
  }) ?~ "App Master Secret required"
     
}