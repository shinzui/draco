// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Printf = require("bs-platform/lib/js/printf.js");
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

function $$escape(s) {
  return Curry._1(Printf.sprintf(/* Format */[
                  /* Caml_string */Block.__(3, [
                      /* No_padding */0,
                      /* End_of_format */0
                    ]),
                  "%S"
                ]), s);
}

var $$delete = function (key,obj){delete obj[key];};

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
            37,
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
exports.$$escape = $$escape;
exports.$$delete = $$delete;
exports.Json = Json;
/* parse Not a pure module */
