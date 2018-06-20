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
  var instance = Curry._2(Gcloud$LidcoreDraco.Compute[/* Zone */4][/* vm */7], Gcloud$LidcoreDraco.Compute[/* zone */5](Gcloud$LidcoreDraco.Compute[/* init */0](/* None */0, /* () */0), zone), Os$LidcoreBsNode.hostname(/* () */0));
  return BsAsyncMonad.Callback[/* finish */27](/* Some */[exceptionHandler], BsAsyncMonad.Callback[/* >> */3](Curry._1(Gcloud$LidcoreDraco.Compute[/* Zone */4][/* VM */6][/* getMetadata */0], instance), (function (meta) {
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
  return BsAsyncMonad.Callback[/* >> */3]((function (param) {
                return Redis$LidcoreDraco.setnx(redis, key, "foo", param);
              }), (function (n) {
                if (n === 0) {
                  var partial_arg = BsAsyncMonad.Callback[/* return */0];
                  return (function (param) {
                      return partial_arg(true, param);
                    });
                } else {
                  return BsAsyncMonad.Callback[/* >| */7]((function (param) {
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
    return BsAsyncMonad.Callback[/* ||> */5](requeue(msg), (function (exn) {
                  Curry._1(Config$LidcoreDraco.error_handler[0], exn);
                  var partial_arg = BsAsyncMonad.Callback[/* return */0];
                  return (function (param) {
                      return partial_arg(/* () */0, param);
                    });
                }));
  };
  return BsAsyncMonad.Callback[/* >| */7](Gcloud$LidcoreDraco.PubSub[/* subscription */3](config, topic, pubsub, subscription), (function (s) {
                return Gcloud$LidcoreDraco.PubSub[/* subscribe */6](s, (function (msg) {
                              if (stopping[0]) {
                                return Gcloud$LidcoreDraco.PubSub[/* nack */5](msg);
                              } else {
                                var handler$1 = function () {
                                  return BsAsyncMonad.Callback[/* >| */7](Curry._1(handler, msg.data), (function () {
                                                return Gcloud$LidcoreDraco.PubSub[/* ack */4](msg);
                                              }));
                                };
                                var id = msg.id;
                                var handler$2 = BsAsyncMonad.Callback[/* >> */3](is_duplicate(id), (function (ret) {
                                        if (ret) {
                                          log("Found duplicate message with id: " + (String(id) + ""));
                                          Gcloud$LidcoreDraco.PubSub[/* ack */4](msg);
                                          var partial_arg = BsAsyncMonad.Callback[/* return */0];
                                          return (function (param) {
                                              return partial_arg(/* () */0, param);
                                            });
                                        } else {
                                          return BsAsyncMonad.Callback[/* ||> */5](handler$1(/* () */0), (function (exn) {
                                                        Gcloud$LidcoreDraco.PubSub[/* ack */4](msg);
                                                        return BsAsyncMonad.Callback[/* >> */3](requeue$1(msg), (function () {
                                                                      var partial_arg = BsAsyncMonad.Callback[/* fail */1];
                                                                      return (function (param) {
                                                                          return partial_arg(exn, param);
                                                                        });
                                                                    }));
                                                      }));
                                        }
                                      }));
                                return BsAsyncMonad.Callback[/* finish */27](/* Some */[exceptionHandler], handler$2);
                              }
                            }));
              }));
}

function components(config) {
  var name = config.name;
  var projectId = config.projectId;
  var compute = Gcloud$LidcoreDraco.Compute[/* init */0](/* Some */[{
          projectId: projectId,
          baseUrl: "https://www.googleapis.com/compute/beta"
        }], /* () */0);
  var zone = config.zone;
  var instanceTemplate = Gcloud$LidcoreDraco.Compute[/* instanceTemplate */2](compute, name);
  var zone$1 = Gcloud$LidcoreDraco.Compute[/* zone */5](compute, zone);
  var instanceGroupManager = Curry._2(Gcloud$LidcoreDraco.Compute[/* Zone */4][/* instanceGroupManager */4], zone$1, name);
  var autoscaler = Curry._2(Gcloud$LidcoreDraco.Compute[/* Zone */4][/* autoscaler */1], zone$1, name);
  return /* record */[
          /* name */name,
          /* compute */compute,
          /* zone */zone$1,
          /* instanceTemplate */instanceTemplate,
          /* instanceGroupManager */instanceGroupManager,
          /* autoscaler */autoscaler
        ];
}

function initialize(config) {
  var components$1 = components(config);
  var name = components$1[/* name */0];
  var serviceAccount = config.serviceAccount;
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
  autoscaleConfig.zone = (function (prim) {
      return prim.zone;
    });
  var autoscaler = components$1[/* autoscaler */5];
  var instanceGroupManager = components$1[/* instanceGroupManager */4];
  var instanceTemplate = components$1[/* instanceTemplate */3];
  var zone = components$1[/* zone */2];
  var compute = components$1[/* compute */1];
  var createInstanceTemplate = function () {
    var partial_arg = Curry._1(Gcloud$LidcoreDraco.Compute[/* InstanceTemplate */1][/* exists */0], instanceTemplate);
    var partial_arg$1 = BsAsyncMonad.Callback[/* async_unless */14];
    return (function (param) {
        return partial_arg$1(partial_arg, (function () {
                      var partial_arg = Gcloud$LidcoreDraco.Compute[/* createInstanceTemplate */3];
                      return BsAsyncMonad.Callback[/* >> */3]((function (param) {
                                    return partial_arg(compute, name, instanceTemplateConfig, param);
                                  }), Gcloud$LidcoreDraco.Compute[/* InstanceTemplate */1][/* get */1]);
                    }), param);
      });
  };
  var createGroup = function () {
    var partial_arg = Curry._1(Gcloud$LidcoreDraco.Compute[/* Zone */4][/* InstanceGroupManager */3][/* exists */0], instanceGroupManager);
    var partial_arg$1 = BsAsyncMonad.Callback[/* async_unless */14];
    return (function (param) {
        return partial_arg$1(partial_arg, (function () {
                      return BsAsyncMonad.Callback[/* >> */3](Curry._5(Gcloud$LidcoreDraco.Compute[/* Zone */4][/* createInstanceGroupManager */5], /* None */0, 0, instanceTemplate, zone, name), Gcloud$LidcoreDraco.Compute[/* Zone */4][/* InstanceGroupManager */3][/* get */1]);
                    }), param);
      });
  };
  var createAutoscaler = function () {
    var partial_arg = Curry._1(Gcloud$LidcoreDraco.Compute[/* Zone */4][/* Autoscaler */0][/* exists */0], autoscaler);
    var partial_arg$1 = BsAsyncMonad.Callback[/* async_unless */14];
    return (function (param) {
        return partial_arg$1(partial_arg, (function () {
                      var partial_arg = Curry._3(Gcloud$LidcoreDraco.Compute[/* Zone */4][/* createAutoscaler */2], zone, name, autoscaleConfig);
                      var partial_arg$1 = BsAsyncMonad.Callback[/* discard */10];
                      return (function (param) {
                          return partial_arg$1(partial_arg, param);
                        });
                    }), param);
      });
  };
  return BsAsyncMonad.Callback[/* >> */3](BsAsyncMonad.Callback[/* >> */3](createInstanceTemplate(/* () */0), createGroup), createAutoscaler);
}

function restart(config) {
  var match = components(config);
  return Curry._1(Gcloud$LidcoreDraco.Compute[/* Zone */4][/* InstanceGroupManager */3][/* recreateVMs */3], match[/* instanceGroupManager */4]);
}

function destroy(config) {
  var match = components(config);
  var autoscaler = match[/* autoscaler */5];
  var instanceGroupManager = match[/* instanceGroupManager */4];
  var instanceTemplate = match[/* instanceTemplate */3];
  var partial_arg = Curry._1(Gcloud$LidcoreDraco.Compute[/* Zone */4][/* Autoscaler */0][/* exists */0], autoscaler);
  var partial_arg$1 = BsAsyncMonad.Callback[/* async_if */13];
  var deleteAutoscaler = function (param) {
    return partial_arg$1(partial_arg, (function () {
                  var partial_arg = Curry._1(Gcloud$LidcoreDraco.Compute[/* Zone */4][/* Autoscaler */0][/* delete */1], autoscaler);
                  var partial_arg$1 = BsAsyncMonad.Callback[/* discard */10];
                  return (function (param) {
                      return partial_arg$1(partial_arg, param);
                    });
                }), param);
  };
  var deleteGroup = function () {
    var partial_arg = Curry._1(Gcloud$LidcoreDraco.Compute[/* Zone */4][/* InstanceGroupManager */3][/* exists */0], instanceGroupManager);
    var partial_arg$1 = BsAsyncMonad.Callback[/* async_if */13];
    return (function (param) {
        return partial_arg$1(partial_arg, (function () {
                      var partial_arg = Curry._1(Gcloud$LidcoreDraco.Compute[/* Zone */4][/* InstanceGroupManager */3][/* delete */2], instanceGroupManager);
                      var partial_arg$1 = BsAsyncMonad.Callback[/* discard */10];
                      return (function (param) {
                          return partial_arg$1(partial_arg, param);
                        });
                    }), param);
      });
  };
  var deleteInstanceTemplate = function () {
    var partial_arg = Curry._1(Gcloud$LidcoreDraco.Compute[/* InstanceTemplate */1][/* exists */0], instanceTemplate);
    var partial_arg$1 = BsAsyncMonad.Callback[/* async_if */13];
    return (function (param) {
        return partial_arg$1(partial_arg, (function () {
                      var partial_arg = Curry._1(Gcloud$LidcoreDraco.Compute[/* InstanceTemplate */1][/* delete */2], instanceTemplate);
                      var partial_arg$1 = BsAsyncMonad.Callback[/* discard */10];
                      return (function (param) {
                          return partial_arg$1(partial_arg, param);
                        });
                    }), param);
      });
  };
  return BsAsyncMonad.Callback[/* >> */3](BsAsyncMonad.Callback[/* >> */3](deleteAutoscaler, deleteGroup), deleteInstanceTemplate);
}

var Runtime = [
  subscribe,
  register,
  run
];

var Config = [
  initialize,
  restart,
  destroy
];

exports.Runtime = Runtime;
exports.Config = Config;
/* instances Not a pure module */
