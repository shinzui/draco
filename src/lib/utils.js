// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Buffer$LidcoreBsNode = require("@lidcore/bs-node/src/buffer.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function get_some(param) {
  if (param) {
    return param[0];
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "utils.ml",
            5,
            12
          ]
        ];
  }
}

function partition(size, a) {
  var len = a.length;
  var ret = /* array */[];
  var pos = 0;
  while(pos < len) {
    ret.push(a.slice(pos, pos + size | 0));
    pos = pos + size | 0;
  };
  return ret;
}

var parse = (function (x) {
    return JSON.parse(x);
  });

function parse_buf(buf) {
  return Curry._1(parse, Buffer$LidcoreBsNode.toString(/* None */0, /* None */0, /* None */0, buf));
}

function stringify(obj) {
  var match = JSON.stringify(obj);
  if (match !== undefined) {
    return match;
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "utils.ml",
            30,
            16
          ]
        ];
  }
}

var Json = /* module */[
  /* parse */parse,
  /* parse_buf */parse_buf,
  /* stringify */stringify
];

exports.get_some = get_some;
exports.partition = partition;
exports.Json = Json;
/* parse Not a pure module */