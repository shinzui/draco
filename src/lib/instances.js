// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Hashtbl = require("bs-platform/lib/js/hashtbl.js");
var BsAsyncMonad = require("bs-async-monad/src/bsAsyncMonad.js");
var Env$LidcoreDraco = require("./env.js");
var Os$LidcoreBsNode = require("@lidcore/bs-node/src/os.js");
var Redis$LidcoreDraco = require("../bindings/redis.js");
var Utils$LidcoreDraco = require("./utils.js");
var Common$LidcoreDraco = require("./common.js");
var Config$LidcoreDraco = require("../config.js");
var Gcloud$LidcoreDraco = require("../bindings/gcloud.js");
var Logger$LidcoreDraco = require("./logger.js");
var Process$LidcoreBsNode = require("@lidcore/bs-node/src/process.js");

function exceptionHandler(exn) {
  return Curry._1(Config$LidcoreDraco.error_handler[0], exn);
}

var instances = Hashtbl.create(/* None */0, 10);

function register(label, handler) {
  return Hashtbl.add(instances, label, handler);
}

function run() {
  var zone = Env$LidcoreDraco.get(/* Some */["us-west1-b"], "zone");
  var instance = Curry._2(Gcloud$LidcoreDraco.Compute[/* Zone */3][/* vm */7], Gcloud$LidcoreDraco.Compute[/* zone */4](Gcloud$LidcoreDraco.Compute[/* init */0](/* None */0, /* () */0), zone), Os$LidcoreBsNode.hostname(/* () */0));
  return BsAsyncMonad.Callback[/* finish */26](/* Some */[exceptionHandler], BsAsyncMonad.Callback[/* >> */5](Curry._1(Gcloud$LidcoreDraco.Compute[/* Zone */3][/* VM */6][/* getMetadata */0], instance), (function (meta) {
                    var items = $$Array.to_list(meta.metadata.items);
                    var data = List.find((function (el) {
                            return el.key === "draco_instance_type";
                          }), items);
                    var label = data.value;
                    Logger$LidcoreDraco.info("Starting instance " + (String(label) + ""));
                    var handler = Hashtbl.find(instances, label);
                    return Curry._1(handler, /* () */0);
                  })));
}

var stopping = [false];

Process$LidcoreBsNode.on(/* SIGTERM */-995060003, (function () {
        stopping[0] = true;
        return /* () */0;
      }));

var msg_check_expire = 60 * 24 * 2;

function msg_check_key(id) {
  return "msg_check_" + (String(id) + "");
}

var redis_client = [/* None */0];

function get_redis() {
  var match = redis_client[0];
  if (match) {
    return match[0];
  } else {
    var client = Redis$LidcoreDraco.createClient(Env$LidcoreDraco.get(/* Some */["redis://localhost:6379"], "REDIS_URL"));
    redis_client[0] = /* Some */[client];
    return client;
  }
}

function is_duplicate(id) {
  var redis = get_redis(/* () */0);
  var key = msg_check_key(id);
  return BsAsyncMonad.Callback[/* >> */5]((function (param) {
                return Redis$LidcoreDraco.setnx(redis, key, "foo", param);
              }), (function (n) {
                if (n === 0) {
                  var partial_arg = BsAsyncMonad.Callback[/* return */2];
                  return (function (param) {
                      return partial_arg(true, param);
                    });
                } else {
                  return BsAsyncMonad.Callback[/* >| */9]((function (param) {
                                return Redis$LidcoreDraco.expire(redis, key, msg_check_expire, param);
                              }), (function () {
                                return false;
                              }));
                }
              }));
}

function log(msg) {
  return Logger$LidcoreDraco.info("" + (String(msg) + ""));
}

function subscribe(maxMessages, topic, subscription, handler) {
  var pubsub = Gcloud$LidcoreDraco.PubSub[/* init */0](/* None */0, /* () */0);
  var flowControl = {
    maxMessages: maxMessages
  };
  var config = {
    flowControl: flowControl
  };
  var requeue = function (msg) {
    var msg$1 = Utils$LidcoreDraco.Json[/* parse_buf */1](msg.data);
    return Common$LidcoreDraco.requeue(msg$1, topic);
  };
  var requeue$1 = function (msg) {
    return BsAsyncMonad.Callback[/* ||> */7](requeue(msg), (function (exn) {
                  Curry._1(Config$LidcoreDraco.error_handler[0], exn);
                  var partial_arg = BsAsyncMonad.Callback[/* return */2];
                  return (function (param) {
                      return partial_arg(/* () */0, param);
                    });
                }));
  };
  return BsAsyncMonad.Callback[/* >| */9](Gcloud$LidcoreDraco.PubSub[/* subscription */3](config, topic, pubsub, subscription), (function (s) {
                return Gcloud$LidcoreDraco.PubSub[/* subscribe */6](s, (function (msg) {
                              if (stopping[0]) {
                                return Gcloud$LidcoreDraco.PubSub[/* nack */5](msg);
                              } else {
                                var handler$1 = function () {
                                  return BsAsyncMonad.Callback[/* >| */9](Curry._1(handler, msg.data), (function () {
                                                return Gcloud$LidcoreDraco.PubSub[/* ack */4](msg);
                                              }));
                                };
                                var id = msg.id;
                                var handler$2 = BsAsyncMonad.Callback[/* >> */5](is_duplicate(id), (function (ret) {
                                        if (ret) {
                                          log("Found duplicate message with id: " + (String(id) + ""));
                                          Gcloud$LidcoreDraco.PubSub[/* ack */4](msg);
                                          var partial_arg = BsAsyncMonad.Callback[/* return */2];
                                          return (function (param) {
                                              return partial_arg(/* () */0, param);
                                            });
                                        } else {
                                          return BsAsyncMonad.Callback[/* ||> */7](handler$1(/* () */0), (function (exn) {
                                                        Gcloud$LidcoreDraco.PubSub[/* ack */4](msg);
                                                        return BsAsyncMonad.Callback[/* >> */5](requeue$1(msg), (function () {
                                                                      var partial_arg = BsAsyncMonad.Callback[/* fail */3];
                                                                      return (function (param) {
                                                                          return partial_arg(exn, param);
                                                                        });
                                                                    }));
                                                      }));
                                        }
                                      }));
                                return BsAsyncMonad.Callback[/* finish */26](/* Some */[exceptionHandler], handler$2);
                              }
                            }));
              }));
}

function initialize($staropt$star, config) {
  var restart = $staropt$star ? $staropt$star[0] : true;
  var name = config.name;
  var zone = config.zone;
  var serviceAccount = config.serviceAccount;
  var projectId = config.projectId;
  var instanceTemplateConfig = config.instanceTemplate;
  instanceTemplateConfig.properties.metadata = {
    items: /* array */[{
        key: "mode",
        value: name
      }]
  };
  instanceTemplateConfig.properties.serviceAccounts = /* array */[{
      email: serviceAccount,
      scopes: /* array */["https://www.googleapis.com/auth/cloud-platform"]
    }];
  var autoscaleConfig = config.autoscale;
  autoscaleConfig.target = name;
  autoscaleConfig.name = name;
  autoscaleConfig.zone = zone;
  var compute = Gcloud$LidcoreDraco.Compute[/* init */0](/* Some */[{
          projectId: projectId
        }], /* () */0);
  var instanceTemplate = Gcloud$LidcoreDraco.Compute[/* instanceTemplate */2](compute, name);
  var zone$1 = Gcloud$LidcoreDraco.Compute[/* zone */4](compute, zone);
  var instanceGroupManager = Curry._2(Gcloud$LidcoreDraco.Compute[/* Zone */3][/* instanceGroupManager */4], zone$1, name);
  var autoscaler = Curry._2(Gcloud$LidcoreDraco.Compute[/* Zone */3][/* autoscaler */1], zone$1, name);
  var createInstanceTemplate = function () {
    return Curry._2(Gcloud$LidcoreDraco.Compute[/* InstanceTemplate */1][/* get */0], /* Some */[instanceTemplateConfig], instanceTemplate);
  };
  var createGroup = function () {
    return BsAsyncMonad.Callback[/* >> */5](Curry._1(Gcloud$LidcoreDraco.Compute[/* Zone */3][/* InstanceGroupManager */3][/* exists */0], instanceGroupManager), (function (exists) {
                  if (exists) {
                    var partial_arg = BsAsyncMonad.Callback[/* return */2];
                    return (function (param) {
                        return partial_arg(/* () */0, param);
                      });
                  } else {
                    var partial_arg$1 = Curry._5(Gcloud$LidcoreDraco.Compute[/* Zone */3][/* createInstanceGroupManager */5], /* None */0, 0, instanceTemplate, zone$1, name);
                    var partial_arg$2 = BsAsyncMonad.Callback[/* discard */12];
                    return (function (param) {
                        return partial_arg$2(partial_arg$1, param);
                      });
                  }
                }));
  };
  var createAutoscaler = function () {
    return BsAsyncMonad.Callback[/* >> */5](Curry._1(Gcloud$LidcoreDraco.Compute[/* Zone */3][/* Autoscaler */0][/* exists */0], autoscaler), (function (exists) {
                  if (exists) {
                    var partial_arg = BsAsyncMonad.Callback[/* return */2];
                    return (function (param) {
                        return partial_arg(/* () */0, param);
                      });
                  } else {
                    var partial_arg$1 = Curry._3(Gcloud$LidcoreDraco.Compute[/* Zone */3][/* createAutoscaler */2], zone$1, name, autoscaleConfig);
                    var partial_arg$2 = BsAsyncMonad.Callback[/* discard */12];
                    return (function (param) {
                        return partial_arg$2(partial_arg$1, param);
                      });
                  }
                }));
  };
  return BsAsyncMonad.Callback[/* >> */5](BsAsyncMonad.Callback[/* >> */5](BsAsyncMonad.Callback[/* >> */5](createInstanceTemplate(/* () */0), createGroup), createAutoscaler), (function () {
                if (restart) {
                  return Curry._1(Gcloud$LidcoreDraco.Compute[/* Zone */3][/* InstanceGroupManager */3][/* recreateVMs */2], instanceGroupManager);
                } else {
                  var partial_arg = BsAsyncMonad.Callback[/* return */2];
                  return (function (param) {
                      return partial_arg(/* () */0, param);
                    });
                }
              }));
}

var Config = /* module */[/* initialize */initialize];

var Runtime = [
  subscribe,
  register,
  run
];

exports.Runtime = Runtime;
exports.Config = Config;
/* instances Not a pure module */
