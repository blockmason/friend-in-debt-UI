// Generated by purs version 0.11.5
"use strict";
var Data_Eq = require("../Data.Eq");
var Data_Newtype = require("../Data.Newtype");
var Data_Ord = require("../Data.Ord");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Data_String = require("../Data.String");
var Prelude = require("../Prelude");
var CaseInsensitiveString = function (x) {
    return x;
};
var showCaseInsensitiveString = new Data_Show.Show(function (v) {
    return "(CaseInsensitiveString " + (v + ")");
});
var newtypeCaseInsensitiveString = new Data_Newtype.Newtype(function (n) {
    return n;
}, CaseInsensitiveString);
var eqCaseInsensitiveString = new Data_Eq.Eq(function (v) {
    return function (v1) {
        return Data_String.toLower(v) === Data_String.toLower(v1);
    };
});
var ordCaseInsensitiveString = new Data_Ord.Ord(function () {
    return eqCaseInsensitiveString;
}, function (v) {
    return function (v1) {
        return Data_Ord.compare(Data_Ord.ordString)(Data_String.toLower(v))(Data_String.toLower(v1));
    };
});
module.exports = {
    CaseInsensitiveString: CaseInsensitiveString, 
    eqCaseInsensitiveString: eqCaseInsensitiveString, 
    ordCaseInsensitiveString: ordCaseInsensitiveString, 
    showCaseInsensitiveString: showCaseInsensitiveString, 
    newtypeCaseInsensitiveString: newtypeCaseInsensitiveString
};
