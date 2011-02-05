package com.bubblefoundry.bfurbanairship

import dispatch._
import dispatch.liftjson.Js._
import net.liftweb.json.JsonAST._
import net.liftweb.json.Serialization.write

import net.liftweb.common.Box
import net.liftweb.util.Helpers


object LiftJsonHelpers {
  implicit val formats = new net.liftweb.json.DefaultFormats {
    override def dateFormatter = {
      val format = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
      // ensure we're using UTC as our output
      format.setTimeZone(java.util.TimeZone.getTimeZone("UTC"))
      format
    }
  }
}

case class Quiettime(start: String, end: String)
case class Device(device_token: String, alias: Option[String], tags: Option[List[String]], badge: Option[Int], quiettime: Option[Quiettime], tz: Option[String], last_registration: Option[java.util.Date], active: Option[Boolean], marked_inactive_on: Option[java.util.Date])
object Device {
  def apply(device_token: String): Device = Device(device_token, None, None, None, None, None, None, None, None)
  def apply(device_token: String, alias: String): Device = Device(device_token, Some(alias), None, None, None, None, None, None, None)
  def apply(device_token: String, alias: Option[String], tags: Option[List[String]], badge: Option[Int], quiettime: Option[Quiettime], tz: Option[String]): Device = Device(device_token, alias, tags, badge, quiettime, tz, None, None, None)
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


// like with APS, allow two types of PushMessages: ones with a List of Dates and ones with a List of alias + Date objects
abstract trait PushMessage[T <: APS]
case class SimplePushMessage[T <: APS](device_tokens: Option[List[String]], aliases: Option[List[String]], tags: Option[List[String]], schedule_for: Option[List[java.util.Date]], exclude_tokens: Option[List[String]], aps: Option[T]) extends PushMessage[T]
object SimplePushMessage {
  private def optionalList[T](l: List[T]) = if (!l.isEmpty) Some(l) else None
  def apply[T <: APS](device_tokens: List[String], aliases: List[String], tags: List[String], schedule_for: List[java.util.Date], exclude_tokens: List[String], aps: T): SimplePushMessage[T] = SimplePushMessage(optionalList(device_tokens), optionalList(aliases), optionalList(tags), optionalList(schedule_for), optionalList(exclude_tokens), Some(aps))
  def apply[T <: APS](device_tokens: List[String], schedule_for: List[java.util.Date], aps: T): SimplePushMessage[T] = SimplePushMessage(optionalList(device_tokens), None, None, optionalList(schedule_for), None, Some(aps))
}
case class AliasedSchedule(alias: String, scheduled_time: java.util.Date)
case class AliasedPushMessage[T <: APS](device_tokens: Option[List[String]], aliases: Option[List[String]], tags: Option[List[String]], schedule_for: Option[List[AliasedSchedule]], exclude_tokens: Option[List[String]], aps: Option[T]) extends PushMessage[T]
object AliasedPushMessage {
  private def optionalList[T](l: List[T]) = if (!l.isEmpty) Some(l) else None
  def apply[T <: APS](device_tokens: List[String], aliases: List[String], tags: List[String], schedule_for: List[AliasedSchedule], exclude_tokens: List[String], aps: T): AliasedPushMessage[T] = AliasedPushMessage(optionalList(device_tokens), optionalList(aliases), optionalList(tags), optionalList(schedule_for), optionalList(exclude_tokens), Some(aps))
  def apply[T <: APS](device_tokens: List[String], schedule_for: List[AliasedSchedule], aps: T): AliasedPushMessage[T] = AliasedPushMessage(optionalList(device_tokens), None, None, optionalList(schedule_for), None, Some(aps))
}


case class ScheduledMessage[M <: PushMessage[_]](alias: Option[String], schedule_for: java.util.Date, payload: M)
case class ScheduledPushes(scheduled_notifications: List[String])

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
case class DeleteScheduledMessages(cancel: List[String], cancel_aliases: List[String], cancel_device_tokens: List[String])

case class HourlyStatistics(start: java.util.Date, messages: Int, android_messages: Int, bb_messages: Int)

class UrbanAirship(app_token: String, app_secret: Box[String], app_master_secret: Box[String]) {  
  lazy val http = new Http
  lazy val urbanairshipReq = :/("go.urbanairship.com").secure
  lazy val apiReq = urbanairshipReq / "api"
  lazy val pushReq = apiReq / "push"

  lazy val devicesReq = apiReq / "device_tokens"
  lazy val feedbackReq = devicesReq / "feedback"
  lazy val scheduledReq = pushReq / "scheduled"
  lazy val statisticsReq = pushReq / "stats"
  
  def this(app_token: String, app_secret: Option[String], app_master_secret: Option[String]) = {
    this(app_token, Box(app_secret), Box(app_master_secret))
  }

  def register_device(device: Device): Box[Device] = app_secret.flatMap(secret => {
    import LiftJsonHelpers._
    val req = devicesReq / device.device_token <<< write(device) <:< Map("Content-Type" -> "application/json") as (app_token, secret)
    Helpers.tryo(http(req ># (json => {
      json.extract[Device] // then update the returned object with its tags, since it apparently doesn't return them?
    })))
  }) ?~ "App Secret required"
  
  // GET device 
  def device(device_token: String): Box[Device] = app_master_secret.flatMap(secret => {
    import LiftJsonHelpers._
    val req = devicesReq / device_token as (app_token, secret)
    Helpers.tryo(http(req ># (json => {
      json.extract[Device]
    })))    
  }) ?~ "App Master Secret Required"
  def device(d: Device): Box[Device] = device(d.device_token)
  
  // get device tokens
  def devices: Box[Stream[Device]] = app_master_secret.flatMap(secret => {
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
      // turn the first page's List[Device] into an Stream[Device]
      page.device_tokens.toStream ++ ({
          // then for any additional pages (Stream.range(2, 1) == Stream.empty)
          // take the page and be ready to reduce down our inner Stream
          Stream.range(2, page.num_pages).flatMap(p => {
            // get the page, then just its List[Device], escape the Box, to an Stream
            getPage(p).map(_.device_tokens).getOrElse(Nil).toStream
          })
        })
    })
  }) ?~ "App Master Secret Required"
  
  // feedback service
  def feedback(since: String): Box[List[Device]] =  app_master_secret.flatMap(secret => {
    val req = feedbackReq / "" <<? Map("since" -> since) as (app_token, secret) 
    Helpers.tryo(http(req ># (json => {
      import LiftJsonHelpers._
      json.children.map(_.extract[Device])
    })))
  }) ?~ "App Master Secret Required"
  
  
  def devices_count: Box[DevicesCount] = app_master_secret.flatMap(secret => {
    import LiftJsonHelpers._
    val req = devicesReq / "count" / "" as (app_token, secret)
    Helpers.tryo(http(req ># (_.extract[DevicesCount])))
  }) ?~ "App Master Secret Required"
  
  def push[M <: PushMessage[_]](message: M): Box[ScheduledPushes] = app_master_secret.flatMap(secret => {
    import LiftJsonHelpers._
    val req = pushReq / "" << write(message) <:< Map("Content-Type" -> "application/json") as (app_token, secret)
    Helpers.tryo(http(req ># (json => json.extract[ScheduledPushes])))
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
  def pushMany[M <: PushMessage[_]](messages: List[M]): Box[String] = app_master_secret.flatMap(secret => {
    import LiftJsonHelpers._
    val req = pushReq / "batch" / "" << write(messages) <:< Map("Content-Type" -> "application/json") as (app_token, secret)
    Helpers.tryo(http(req as_str))
  }) ?~ "App Master Secret Required"
  
  // schedule_for should be a string here, not a list of strings? Though UA seems to allow a string in places where you'd otherwise have a list of one.
  def pushAll[M <: PushMessage[_]](message: M): Box[String] = app_master_secret.flatMap(secret => {
    import LiftJsonHelpers._
    val req = pushReq / "broadcast" / "" << write(message) <:< Map("Content-Type" -> "application/json") as (app_token, secret)
    Helpers.tryo(http(req as_str))
  }) ?~ "App Master Secret Required"
   
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
  def update_scheduled[M <: PushMessage[_]](alias: String, message: ScheduledMessage[M]): Box[String] =  app_master_secret.flatMap(secret => {
    import LiftJsonHelpers._
    val req = scheduledReq / "alias" / alias <<< write(message) as (app_token, secret) 
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
  def delete_scheduled(cancel: DeleteScheduledMessages): Box[String] = app_master_secret.flatMap(secret => {
    import LiftJsonHelpers._
    val req = scheduledReq / "" << write(cancel) as (app_token, secret)
    Helpers.tryo(http(req as_str))
  }) ?~ "App Master Secret Required"
  
  def delete_scheduled(alias: String): Box[Unit] = app_master_secret.flatMap(secret => {
    val req = scheduledReq.DELETE / "alias" / alias as (app_token, secret)
    Helpers.tryo(http(req >|))
  }) ?~ "App Master Secret Required"
  
  def statistics(start: String, end: String): Box[List[HourlyStatistics]] = app_master_secret.flatMap(secret => {
    val req = statisticsReq <<? Map("start" -> start, "end" -> end) as (app_token, secret) 
    Helpers.tryo(http(req ># (json => {
      import LiftJsonHelpers._
      json.children.map(_.extract[HourlyStatistics])
    })))
  }) ?~ "App Master Secret required"
  
  /* TAGS */
  
  /*
    {
      "tags": [
          "tag1",
          "some_tag",
          "portland_or"
      ]
    }
  */
  // /api/tags/
  
  // create tag w/o association to device
  /* HTTP PUT to /api/tags/<tag> */
  
  // In order to entirely remove a tag, send an HTTP DELETE to /api/tags/<tag>. The status code on success will be 204.
  // If the tag has already been removed, the status code will be 404.
  
  // add or remove devices on tags
  /*
    HTTP POST to /api/tags/<tag> with a content type of application/json and a JSON payload will add or remove device tokens from that tag. If the tag doesn’t exist, it will be created.
    Example:    
    {
        "device_tokens": {
            "add": [
                "device_token_1_to_add",
                "device_token_2_to_add"
            ],
            "remove": [
                "device_token_to_remove"
            ]
        }
    }
  */
  
  // HTTP GET to /api/device_tokens/<device_token>/tags/
  
  // HTTP PUT to /api/device_tokens/<device_token>/tags/<tag>
  
  // HTTP DELETE to /api/device_tokens/<device_token>/tags/<tag>
}