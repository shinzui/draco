// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var CliSpinner = require("cli-spinner");
var Js_null_undefined = require("bs-platform/lib/js/js_null_undefined.js");

function stop(clean, t) {
  t.stop(Js_null_undefined.fromOption(clean));
  return /* () */0;
}

function init(prim) {
  return new CliSpinner.Spinner(prim);
}

function start(prim) {
  prim.start();
  return /* () */0;
}

exports.init = init;
exports.start = start;
exports.stop = stop;
/* cli-spinner Not a pure module */
