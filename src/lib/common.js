// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var BsAsyncMonad = require("bs-async-monad/src/bsAsyncMonad.js");
var Utils$LidcoreDraco = require("./utils.js");
var Config$LidcoreDraco = require("../config.js");
var Gcloud$LidcoreDraco = require("../bindings/gcloud.js");
var Logger$LidcoreDraco = require("./logger.js");
var JsError$LidcoreDraco = require("../bindings/jsError.js");

var pubsub = Gcloud$LidcoreDraco.PubSub[/* init */0](/* None */0, /* () */0);

function requeue(msg, topic) {
  var match = msg.retry;
  var retry = (match == null) ? 1 : match + 1 | 0;
  msg.retry = retry;
  var msg$1 = Utils$LidcoreDraco.Json[/* stringify */2](msg);
  if (retry >= Config$LidcoreDraco.maxMessageRetries[0]) {
    var partial_arg = JsError$LidcoreDraco.make("[" + (String(topic) + ("] Message reached max retries: " + (String(msg$1) + ""))));
    var partial_arg$1 = BsAsyncMonad.Callback[/* fail */1];
    return (function (param) {
        return partial_arg$1(partial_arg, param);
      });
  } else {
    Logger$LidcoreDraco.info("Retrying message: " + (String(msg$1) + ""));
    return Gcloud$LidcoreDraco.PubSub[/* publish */1](pubsub, topic, msg$1);
  }
}

exports.requeue = requeue;
/* pubsub Not a pure module */
