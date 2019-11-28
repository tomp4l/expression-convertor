// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Future = require("reason-future/src/Future.bs.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");

var fullInput = /* record */[/* contents */""];

process.stdin.resume();

process.stdin.setEncoding("utf-8");

process.stdin.on("data", (function (data) {
        fullInput[0] = fullInput[0] + data;
        return /* () */0;
      }));

var readInput = Future.make((function (resolve) {
        process.stdin.on("end", (function (param) {
                var tmp;
                try {
                  tmp = Caml_option.some(JSON.parse(fullInput[0]));
                }
                catch (exn){
                  tmp = undefined;
                }
                return Curry._1(resolve, tmp);
              }));
        return /* () */0;
      }));

exports.readInput = readInput;
/*  Not a pure module */
