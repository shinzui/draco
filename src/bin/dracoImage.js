// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Js_exn = require("bs-platform/lib/js/js_exn.js");
var $$String = require("bs-platform/lib/js/string.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Fs$LidcoreBsNode = require("@lidcore/bs-node/src/fs.js");
var Tmp$LidcoreDraco = require("../bindings/tmp.js");
var Cuid$LidcoreDraco = require("../bindings/cuid.js");
var Utils$LidcoreDraco = require("../lib/utils.js");
var Process$LidcoreBsNode = require("@lidcore/bs-node/src/process.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");
var DracoCommon$LidcoreDraco = require("./dracoCommon.js");
var Child_process$LidcoreBsNode = require("@lidcore/bs-node/src/child_process.js");

DracoCommon$LidcoreDraco.usage("image build [base|app|both]");

function getConfig(config, mode, name) {
  var match = config.image;
  if (match == null) {
    throw Caml_builtin_exceptions.not_found;
  } else {
    var match$1 = match[mode];
    if (match$1 !== undefined) {
      var match$2 = match$1[name];
      if (match$2 !== undefined) {
        return match$2;
      } else {
        throw Caml_builtin_exceptions.not_found;
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  }
}

function provisioners(config, mode) {
  var get = function (mode) {
    var ret = getConfig(config, mode, "provisioners");
    $$Array.sort((function (x, y) {
            return -Caml_obj.caml_compare(x.priority, y.priority) | 0;
          }), ret);
    $$Array.iter((function (param) {
            return Utils$LidcoreDraco.$$delete("priority", param);
          }), ret);
    return ret;
  };
  if (mode === "both") {
    return get("base").concat(get("app"));
  } else {
    return get(mode);
  }
}

function buildConfig(config, mode) {
  var provisioners$1 = provisioners(config, mode);
  var mode$1 = mode === "both" ? "base" : mode;
  var id = Cuid$LidcoreDraco.get(/* () */0);
  var instance_name = "draco-build-" + (String(id) + "");
  var builder = getConfig(config, mode$1, "builder");
  var condSet = function (lbl, value) {
    var match = builder[lbl];
    if (match !== undefined) {
      return /* () */0;
    } else {
      builder[lbl] = value;
      return /* () */0;
    }
  };
  condSet("project_id", config.projectId);
  condSet("zone", config.zone);
  condSet("instance_name", instance_name);
  return {
          provisioners: provisioners$1,
          builders: /* array */[builder]
        };
}

function getConfig$1(tmp, config, mode) {
  var packerConfig = buildConfig(config, mode);
  var path = Tmp$LidcoreDraco.make(/* None */0, /* None */0, /* Some */[".json"], tmp);
  Fs$LidcoreBsNode.writeFileSync(path, Utils$LidcoreDraco.Json[/* stringify */2](packerConfig));
  return path;
}

function packer(args, config, mode) {
  var tmp = Tmp$LidcoreDraco.init(/* None */0, /* () */0);
  var config$1 = Utils$LidcoreDraco.$$escape(getConfig$1(tmp, config, mode));
  var args$1 = List.map((function (param) {
          var opt = Utils$LidcoreDraco.$$escape("" + (String(param[0]) + ("=" + (String(param[1]) + ""))));
          return "-var " + (String(opt) + "");
        }), args);
  var args$2 = $$String.concat(" ", args$1);
  var stdio_000 = /* stdin : `Inherit */[
    -72987685,
    Process$LidcoreBsNode.stdin
  ];
  var stdio_001 = /* stdout : `Inherit */[
    -72987685,
    Process$LidcoreBsNode.stdout
  ];
  var stdio_002 = /* stderr : `Inherit */[
    -72987685,
    Process$LidcoreBsNode.stderr
  ];
  var stdio = /* record */[
    stdio_000,
    stdio_001,
    stdio_002
  ];
  var child = Child_process$LidcoreBsNode.spawn(/* None */0, /* None */0, /* Some */[stdio], /* Some */[true], "packer build -force " + (String(args$2) + (" " + (String(config$1) + ""))));
  return Child_process$LidcoreBsNode.on(child, /* `Exit */[
              771171134,
              (function () {
                  return Tmp$LidcoreDraco.cleanup(tmp);
                })
            ]);
}

if (DracoCommon$LidcoreDraco.argc < 3) {
  DracoCommon$LidcoreDraco.die(/* None */0, /* () */0);
}

if (Caml_array.caml_array_get(DracoCommon$LidcoreDraco.argv, 2) !== "build") {
  DracoCommon$LidcoreDraco.die(/* None */0, /* () */0);
}

var mode;

try {
  var mode$1 = Caml_array.caml_array_get(DracoCommon$LidcoreDraco.argv, 3);
  if (!List.mem(mode$1, /* :: */[
          "base",
          /* :: */[
            "app",
            /* :: */[
              "both",
              /* [] */0
            ]
          ]
        ])) {
    DracoCommon$LidcoreDraco.die(/* Some */["Invalid mode: " + (String(mode$1) + "")], /* () */0);
  }
  mode = mode$1;
}
catch (raw_exn){
  var exn = Js_exn.internalToOCamlException(raw_exn);
  if (exn[0] === Caml_builtin_exceptions.invalid_argument) {
    if (exn[1] === "index out of bounds") {
      mode = "both";
    } else {
      throw exn;
    }
  } else {
    throw exn;
  }
}

var config = DracoCommon$LidcoreDraco.config(/* () */0);

var args_000 = /* tuple */[
  "project",
  config.projectId
];

var args_001 = /* :: */[
  /* tuple */[
    "zone",
    config.zone
  ],
  /* [] */0
];

var args = /* :: */[
  args_000,
  args_001
];

packer(args, config, mode);

/*  Not a pure module */
