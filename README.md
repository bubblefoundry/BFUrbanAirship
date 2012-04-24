# BFUrbanAirship

BFUrbanAirship is a Scala library for the [Urban Airship](http://www.urbanairship.com) [iOS Push Notification API](https://docs.urbanairship.com/display/DOCS/Server%3A+iOS+Push+API).

## Usage

    import com.bubblefoundry.bfurbanairship._
    
    val api = new UrbanAirship(app_token, app_secret, app_master_secret, appengine = false)
    // schedule a message to be pushed
    val message = SimplePushMessage(aps = Some(APS("A push message")))
    api.push(message)
    
    // get a Stream of all registered devices
    api.devices

## Getting the library

Currently you'll need to checkout the source code and build it using [sbt](https://github.com/harrah/xsbt/wiki) **0.7.4**. `sbt publish-local` should do the trick.

## TODO

- Use the latest version of sbt
- Update dependencies
- Clean up the code
- Maybe support the other UA push APIs using a common interface

## License

This code is released under the MIT License. 