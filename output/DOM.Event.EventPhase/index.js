// Generated by purs version 0.11.5
"use strict";
var Data_Bounded = require("../Data.Bounded");
var Data_Enum = require("../Data.Enum");
var Data_Eq = require("../Data.Eq");
var Data_Maybe = require("../Data.Maybe");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Prelude = require("../Prelude");
var None = (function () {
    function None() {

    };
    None.value = new None();
    return None;
})();
var Capturing = (function () {
    function Capturing() {

    };
    Capturing.value = new Capturing();
    return Capturing;
})();
var AtTarget = (function () {
    function AtTarget() {

    };
    AtTarget.value = new AtTarget();
    return AtTarget;
})();
var Bubbling = (function () {
    function Bubbling() {

    };
    Bubbling.value = new Bubbling();
    return Bubbling;
})();
var toEnumEventPhase = function (v) {
    if (v === 0) {
        return new Data_Maybe.Just(None.value);
    };
    if (v === 1) {
        return new Data_Maybe.Just(Capturing.value);
    };
    if (v === 2) {
        return new Data_Maybe.Just(AtTarget.value);
    };
    if (v === 3) {
        return new Data_Maybe.Just(Bubbling.value);
    };
    return Data_Maybe.Nothing.value;
};
var fromEnumEventPhase = function (v) {
    if (v instanceof None) {
        return 0;
    };
    if (v instanceof Capturing) {
        return 1;
    };
    if (v instanceof AtTarget) {
        return 2;
    };
    if (v instanceof Bubbling) {
        return 3;
    };
    throw new Error("Failed pattern match at DOM.Event.EventPhase line 40, column 3 - line 44, column 17: " + [ v.constructor.name ]);
};
var eqEventPhase = new Data_Eq.Eq(function (x) {
    return function (y) {
        if (x instanceof None && y instanceof None) {
            return true;
        };
        if (x instanceof Capturing && y instanceof Capturing) {
            return true;
        };
        if (x instanceof AtTarget && y instanceof AtTarget) {
            return true;
        };
        if (x instanceof Bubbling && y instanceof Bubbling) {
            return true;
        };
        return false;
    };
});
var ordEventPhase = new Data_Ord.Ord(function () {
    return eqEventPhase;
}, function (x) {
    return function (y) {
        if (x instanceof None && y instanceof None) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof None) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof None) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Capturing && y instanceof Capturing) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Capturing) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Capturing) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof AtTarget && y instanceof AtTarget) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof AtTarget) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof AtTarget) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Bubbling && y instanceof Bubbling) {
            return Data_Ordering.EQ.value;
        };
        throw new Error("Failed pattern match at DOM.Event.EventPhase line 14, column 1 - line 14, column 48: " + [ x.constructor.name, y.constructor.name ]);
    };
});
var enumEventPhase = new Data_Enum.Enum(function () {
    return ordEventPhase;
}, Data_Enum.defaultPred(toEnumEventPhase)(fromEnumEventPhase), Data_Enum.defaultSucc(toEnumEventPhase)(fromEnumEventPhase));
var boundedEventPhase = new Data_Bounded.Bounded(function () {
    return ordEventPhase;
}, None.value, Bubbling.value);
var boundedEnumEventPhase = new Data_Enum.BoundedEnum(function () {
    return boundedEventPhase;
}, function () {
    return enumEventPhase;
}, 4, fromEnumEventPhase, toEnumEventPhase);
module.exports = {
    None: None, 
    Capturing: Capturing, 
    AtTarget: AtTarget, 
    Bubbling: Bubbling, 
    fromEnumEventPhase: fromEnumEventPhase, 
    toEnumEventPhase: toEnumEventPhase, 
    eqEventPhase: eqEventPhase, 
    ordEventPhase: ordEventPhase, 
    boundedEventPhase: boundedEventPhase, 
    enumEventPhase: enumEventPhase, 
    boundedEnumEventPhase: boundedEnumEventPhase
};
