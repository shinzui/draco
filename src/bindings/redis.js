// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Redis = require("redis");

function on_error(client, fn) {
  client.on("error", Curry.__1(fn));
  return /* () */0;
}

function createClient(prim) {
  return Redis.createClient(prim);
}

function setnx(prim, prim$1, prim$2, prim$3) {
  prim.setnx(prim$1, prim$2, prim$3);
  return /* () */0;
}

function expire(prim, prim$1, prim$2, prim$3) {
  prim.expire(prim$1, prim$2, prim$3);
  return /* () */0;
}

exports.createClient = createClient;
exports.on_error = on_error;
exports.setnx = setnx;
exports.expire = expire;
/* redis Not a pure module */
