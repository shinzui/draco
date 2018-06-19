// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var BsAsyncMonad = require("bs-async-monad/src/bsAsyncMonad.js");
var Gcloud$LidcoreDraco = require("../bindings/gcloud.js");

var projectId = "komodo-staging-99c51";

var name = "foo";

var compute = Gcloud$LidcoreDraco.Compute[/* init */0](/* Some */[{
        projectId: projectId
      }], /* () */0);

var zone = Gcloud$LidcoreDraco.Compute[/* zone */4](compute, "us-east1-b");

var instanceGroupManager = Curry._2(Gcloud$LidcoreDraco.Compute[/* Zone */3][/* instanceGroupManager */4], zone, name);

BsAsyncMonad.Callback[/* finish */26](/* None */0, Curry._1(Gcloud$LidcoreDraco.Compute[/* Zone */3][/* InstanceGroupManager */3][/* delete */1], instanceGroupManager));

exports.projectId = projectId;
exports.name = name;
exports.compute = compute;
exports.zone = zone;
exports.instanceGroupManager = instanceGroupManager;
/* compute Not a pure module */
