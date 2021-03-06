// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var Caml_sys = require("bs-platform/lib/js/caml_sys.js");
var Dotenv$LidcoreDraco = require("../bindings/dotenv.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

Dotenv$LidcoreDraco.config(/* () */0);

var stage;

try {
  stage = Caml_sys.caml_sys_getenv("STAGE");
}
catch (exn){
  if (exn === Caml_builtin_exceptions.not_found) {
    stage = "dev";
  } else {
    throw exn;
  }
}

var release;

try {
  release = Caml_sys.caml_sys_getenv("RELEASE");
}
catch (exn$1){
  if (exn$1 === Caml_builtin_exceptions.not_found) {
    release = "unknown";
  } else {
    throw exn$1;
  }
}

function env_get(label) {
  try {
    return /* Some */[Caml_sys.caml_sys_getenv(label)];
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      return /* None */0;
    } else {
      throw exn;
    }
  }
}

function get($$default, label) {
  var match = env_get(label);
  if (match) {
    return match[0];
  } else if ($$default) {
    return $$default[0];
  } else {
    throw Caml_builtin_exceptions.not_found;
  }
}

function get_some($$default, label) {
  try {
    return /* Some */[get($$default, label)];
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      return /* None */0;
    } else {
      throw exn;
    }
  }
}

exports.stage = stage;
exports.release = release;
exports.get = get;
exports.get_some = get_some;
/*  Not a pure module */
