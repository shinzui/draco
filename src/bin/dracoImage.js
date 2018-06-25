// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Js_exn = require("bs-platform/lib/js/js_exn.js");
var $$String = require("bs-platform/lib/js/string.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_string = require("bs-platform/lib/js/caml_string.js");
var Js_mapperRt = require("bs-platform/lib/js/js_mapperRt.js");
var Fs$LidcoreBsNode = require("@lidcore/bs-node/src/fs.js");
var Tmp$LidcoreDraco = require("../bindings/tmp.js");
var Cuid$LidcoreDraco = require("../bindings/cuid.js");
var Shell$LidcoreDraco = require("../bindings/shell.js");
var Utils$LidcoreDraco = require("../lib/utils.js");
var Process$LidcoreBsNode = require("@lidcore/bs-node/src/process.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");
var DracoCommon$LidcoreDraco = require("./dracoCommon.js");
var Child_process$LidcoreBsNode = require("@lidcore/bs-node/src/child_process.js");

var jsMapperConstantArray = /* array */[
  /* tuple */[
    3257473,
    "app"
  ],
  /* tuple */[
    736760881,
    "base"
  ],
  /* tuple */[
    737457313,
    "both"
  ]
];

DracoCommon$LidcoreDraco.usage("image build [base|app|both]");

function getPath(file) {
  return Fs$LidcoreBsNode.realpathSync("" + (String(__dirname) + ("/../../" + (String(file) + ""))));
}

function packFiles(tmp, files) {
  var dir = Tmp$LidcoreDraco.make(/* Some */[true], /* None */0, /* None */0, tmp);
  $$Array.iter((function (file) {
          return Shell$LidcoreDraco.cp(/* Some */["-rf"], file, dir);
        }), files);
  return dir;
}

function buildProvisioner(projectId, zone, mode) {
  var script = getPath("packer/" + (String(mode) + ".sh"));
  return {
          type: "shell",
          script: script,
          environment_vars: /* array */[
            "PROJECT=" + (String(projectId) + ""),
            "ZONE=" + (String(zone) + "")
          ]
        };
}

function builder(source_image, source_image_family, image_name, instance_name, projectId, zone) {
  return (function () {
      var tmp = {
        type: "googlecompute",
        project_id: projectId,
        zone: zone,
        ssh_username: "ubuntu",
        image_name: image_name,
        image_family: "draco",
        instance_name: instance_name,
        machine_type: "n1-standard-1",
        disk_size: "50",
        disk_type: "pd-ssd"
      };
      if (source_image_family) {
        tmp.source_image_family = source_image_family[0];
      }
      if (source_image) {
        tmp.source_image = source_image[0];
      }
      return tmp;
    });
}

function getCustom(config, mode, name) {
  var match = config.image;
  if (match == null) {
    return /* array */[];
  } else {
    var match$1 = match[mode];
    if (match$1 !== undefined) {
      var match$2 = match$1[name];
      if (match$2 !== undefined) {
        return $$Array.map((function (prim) {
                      return prim;
                    }), match$2);
      } else {
        return /* array */[];
      }
    } else {
      return /* array */[];
    }
  }
}

var defaultBaseFiles = /* array */[];

var defaultAppFiles = /* array */[
  "package.json",
  "src",
  "bsconfig.json",
  getPath("packer/draco.system.in")
];

function files(config, mode) {
  var customFiles = getCustom(config, mode, "files");
  var defaultFiles;
  switch (mode) {
    case "app" : 
        defaultFiles = defaultAppFiles;
        break;
    case "base" : 
        defaultFiles = defaultBaseFiles;
        break;
    case "both" : 
        defaultFiles = $$Array.append(defaultBaseFiles, defaultAppFiles);
        break;
    default:
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "dracoImage.ml",
              118,
              13
            ]
          ];
  }
  return defaultFiles.concat(customFiles);
}

function provisioners(tmp, projectId, zone, config, mode) {
  var provisioners$1 = getCustom(config, mode, "provisioners");
  provisioners$1.push(buildProvisioner(projectId, zone, mode));
  var filesProvisioner = function () {
    var files$1 = files(config, mode);
    var source = packFiles(tmp, files$1);
    var source$1 = Caml_string.get(source, source.length - 1 | 0) !== /* "/" */47 ? "" + (String(source) + "/") : source;
    return {
            type: "file",
            source: source$1,
            destination: "/home/ubuntu/app"
          };
  };
  var match = Js_mapperRt.revSearch(3, jsMapperConstantArray, mode);
  if (match) {
    var match$1 = match[0];
    if (match$1 !== 3257473) {
      if (match$1 !== 737457313) {
        
      } else {
        provisioners$1.unshift(filesProvisioner(/* () */0));
      }
    } else {
      provisioners$1.unshift(filesProvisioner(/* () */0));
    }
  }
  return provisioners$1;
}

function buildConfig(tmp, config, mode) {
  var projectId = config.projectId;
  var zone = config.zone;
  var smode = Js_mapperRt.binarySearch(3, mode, jsMapperConstantArray);
  var provisioners$1;
  if (mode !== 737457313) {
    provisioners$1 = provisioners(tmp, projectId, zone, config, smode);
  } else {
    var baseProvisioners = provisioners(tmp, projectId, zone, config, "base");
    provisioners$1 = baseProvisioners.concat(provisioners(tmp, projectId, zone, config, "app"));
  }
  var id = Cuid$LidcoreDraco.get(/* () */0);
  var instance_name = "draco-build-" + (String(id) + "");
  var iname = mode !== 737457313 ? smode : "app";
  var image_name = "draco-" + (String(iname) + "");
  var match = mode >= 736760881 ? /* tuple */[
      /* None */0,
      /* Some */["ubuntu-1604-lts"]
    ] : /* tuple */[
      /* Some */["draco-base"],
      /* Some */["draco-images"]
    ];
  var builder$1 = builder(match[0], match[1], image_name, instance_name, projectId, zone)(/* () */0);
  return {
          provisioners: provisioners$1,
          builders: /* array */[builder$1]
        };
}

function getConfig(tmp, config, mode) {
  var packerConfig = buildConfig(tmp, config, mode);
  var path = Tmp$LidcoreDraco.make(/* None */0, /* None */0, /* Some */[".json"], tmp);
  Fs$LidcoreBsNode.writeFileSync(path, Utils$LidcoreDraco.Json[/* stringify */2](packerConfig));
  return path;
}

function packer(args, config, mode) {
  var tmp = Tmp$LidcoreDraco.init(/* None */0, /* () */0);
  var config$1 = Utils$LidcoreDraco.$$escape(getConfig(tmp, config, mode));
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
  var x = Caml_array.caml_array_get(DracoCommon$LidcoreDraco.argv, 3);
  switch (x) {
    case "app" : 
        mode = /* App */3257473;
        break;
    case "base" : 
        mode = /* Base */736760881;
        break;
    case "both" : 
        mode = /* Both */737457313;
        break;
    default:
      mode = DracoCommon$LidcoreDraco.die(/* Some */["Invalid mode: " + (String(x) + "")], /* () */0);
  }
}
catch (raw_exn){
  var exn = Js_exn.internalToOCamlException(raw_exn);
  if (exn[0] === Caml_builtin_exceptions.invalid_argument) {
    if (exn[1] === "index out of bounds") {
      mode = /* Both */737457313;
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
