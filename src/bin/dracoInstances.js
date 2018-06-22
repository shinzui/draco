// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Printf = require("bs-platform/lib/js/printf.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Js_mapperRt = require("bs-platform/lib/js/js_mapperRt.js");
var BsAsyncMonad = require("bs-async-monad/src/bsAsyncMonad.js");
var Spinner$LidcoreDraco = require("../bindings/spinner.js");
var Instances$LidcoreDraco = require("../lib/instances.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");
var DracoCommon$LidcoreDraco = require("./dracoCommon.js");

var jsMapperConstantArray = /* array */[
  /* tuple */[
    -237546278,
    "Destroying"
  ],
  /* tuple */[
    816044828,
    "Creating"
  ],
  /* tuple */[
    938930095,
    "Restarting"
  ]
];

DracoCommon$LidcoreDraco.usage("instances [create|restart|destroy] instance-group-name");

if (DracoCommon$LidcoreDraco.argc !== 4) {
  DracoCommon$LidcoreDraco.die(/* None */0, /* () */0);
}

var match = Caml_array.caml_array_get(DracoCommon$LidcoreDraco.argv, 2);

var operation;

switch (match) {
  case "create" : 
      operation = /* Create */816044828;
      break;
  case "destroy" : 
      operation = /* Destroy */-237546278;
      break;
  case "restart" : 
      operation = /* Restart */938930095;
      break;
  default:
    operation = DracoCommon$LidcoreDraco.die(/* Some */["Invalid mode"], /* () */0);
}

var name = Caml_array.caml_array_get(DracoCommon$LidcoreDraco.argv, 3);

var config = DracoCommon$LidcoreDraco.config(/* () */0);

var projectId = config.projectId;

var serviceAccount = config.serviceAccount;

var zone = config.zone;

var instances = $$Array.to_list(config.instances);

var config$1;

try {
  config$1 = List.find((function (config) {
          return config.name === name;
        }), instances);
}
catch (exn){
  if (exn === Caml_builtin_exceptions.not_found) {
    config$1 = DracoCommon$LidcoreDraco.die(/* Some */["No config for instance group " + (String(name) + (" in " + (String(DracoCommon$LidcoreDraco.configPath) + "")))], /* () */0);
  } else {
    throw exn;
  }
}

function fn() {
  if (operation !== 816044828) {
    if (operation >= 938930095) {
      return Instances$LidcoreDraco.Config[/* restart */1](projectId, zone, name);
    } else {
      return Instances$LidcoreDraco.Config[/* destroy */2](projectId, zone, name);
    }
  } else {
    return Instances$LidcoreDraco.Config[/* initialize */0](projectId, serviceAccount, zone, config$1.instanceTemplate, config$1.autoscaler, name);
  }
}

var operation$1 = Js_mapperRt.binarySearch(3, operation, jsMapperConstantArray);

var spinner = Spinner$LidcoreDraco.init("" + (String(operation$1) + (" " + (String(name) + ".. %s"))));

Spinner$LidcoreDraco.start(spinner);

BsAsyncMonad.Callback[/* finish */27](/* None */0, BsAsyncMonad.Callback[/* &> */9](fn(/* () */0), (function () {
            Spinner$LidcoreDraco.stop(/* Some */[true], spinner);
            Curry._2(Printf.printf(/* Format */[
                      /* String */Block.__(2, [
                          /* No_padding */0,
                          /* Char_literal */Block.__(12, [
                              /* " " */32,
                              /* String */Block.__(2, [
                                  /* No_padding */0,
                                  /* String_literal */Block.__(11, [
                                      ".. done!\n",
                                      /* End_of_format */0
                                    ])
                                ])
                            ])
                        ]),
                      "%s %s.. done!\n"
                    ]), operation$1, name);
            var partial_arg = BsAsyncMonad.Callback[/* return */0];
            return (function (param) {
                return partial_arg(/* () */0, param);
              });
          })));

/*  Not a pure module */
