// Generated by purs version 0.11.5
"use strict";
var $foreign = require("./foreign");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var DOM_Event_Types = require("../DOM.Event.Types");
var Data_Foreign = require("../Data.Foreign");
var Data_Maybe = require("../Data.Maybe");
var Data_Nullable = require("../Data.Nullable");
var Prelude = require("../Prelude");
var item = function (i) {
    return function ($0) {
        return Data_Nullable.toMaybe($foreign._item(i)($0));
    };
};
var eventToTouchEvent = function ($1) {
    return DOM_Event_Types.readTouchEvent(Data_Foreign.toForeign($1));
};
module.exports = {
    eventToTouchEvent: eventToTouchEvent, 
    item: item, 
    altKey: $foreign.altKey, 
    changedTouches: $foreign.changedTouches, 
    clientX: $foreign.clientX, 
    clientY: $foreign.clientY, 
    ctrlKey: $foreign.ctrlKey, 
    length: $foreign.length, 
    metaKey: $foreign.metaKey, 
    pageX: $foreign.pageX, 
    pageY: $foreign.pageY, 
    screenX: $foreign.screenX, 
    screenY: $foreign.screenY, 
    shiftKey: $foreign.shiftKey, 
    target: $foreign.target, 
    targetTouches: $foreign.targetTouches, 
    touches: $foreign.touches
};
