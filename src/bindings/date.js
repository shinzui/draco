// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';


var partial_arg = Date;

function now() {
  return partial_arg.now();
}

function getFullYear(d) {
  return d.getFullYear();
}

function getDate(d) {
  return d.getDate();
}

function getMonth(d) {
  return d.getMonth();
}

function init(prim) {
  return new Date(prim);
}

exports.now = now;
exports.init = init;
exports.getFullYear = getFullYear;
exports.getDate = getDate;
exports.getMonth = getMonth;
/* partial_arg Not a pure module */
