// Generated by purs version 0.11.5
"use strict";
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Boolean = require("../Data.Boolean");
var Data_Maybe = require("../Data.Maybe");
var Global = require("../Global");
var Prelude = require("../Prelude");
var nan = Global.nan;
var $$isNaN = Global["isNaN"];
var $$isFinite = Global["isFinite"];
var infinity = Global.infinity;
var fromString = (function () {
    var check = function (num) {
        if ($$isFinite(num)) {
            return new Data_Maybe.Just(num);
        };
        if (Data_Boolean.otherwise) {
            return Data_Maybe.Nothing.value;
        };
        throw new Error("Failed pattern match at Data.Number line 45, column 5 - line 46, column 39: " + [ num.constructor.name ]);
    };
    return function ($1) {
        return check(Global.readFloat($1));
    };
})();
module.exports = {
    fromString: fromString, 
    infinity: infinity, 
    "isFinite": $$isFinite, 
    "isNaN": $$isNaN, 
    nan: nan
};
