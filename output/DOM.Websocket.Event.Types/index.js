// Generated by purs version 0.11.5
"use strict";
var DOM_Event_Types = require("../DOM.Event.Types");
var Data_Foreign = require("../Data.Foreign");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var readMessageEvent = Data_Foreign.unsafeReadTagged("MessageEvent");
var readCloseEvent = Data_Foreign.unsafeReadTagged("CloseEvent");
var messageEventToEvent = Unsafe_Coerce.unsafeCoerce;
var closeEventToEvent = Unsafe_Coerce.unsafeCoerce;
module.exports = {
    closeEventToEvent: closeEventToEvent, 
    messageEventToEvent: messageEventToEvent, 
    readCloseEvent: readCloseEvent, 
    readMessageEvent: readMessageEvent
};
