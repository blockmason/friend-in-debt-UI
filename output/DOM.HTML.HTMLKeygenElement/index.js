// Generated by purs version 0.11.5
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var DOM = require("../DOM");
var DOM_HTML_Types = require("../DOM.HTML.Types");
var DOM_Node_Types = require("../DOM.Node.Types");
var Data_Functor = require("../Data.Functor");
var Data_Maybe = require("../Data.Maybe");
var Data_Nullable = require("../Data.Nullable");
var Prelude = require("../Prelude");
var form = function ($0) {
    return Data_Functor.map(Control_Monad_Eff.functorEff)(Data_Nullable.toMaybe)($foreign._form($0));
};
module.exports = {
    form: form, 
    autofocus: $foreign.autofocus, 
    challenge: $foreign.challenge, 
    checkValidity: $foreign.checkValidity, 
    disabled: $foreign.disabled, 
    keytype: $foreign.keytype, 
    labels: $foreign.labels, 
    name: $foreign.name, 
    setAutofocus: $foreign.setAutofocus, 
    setChallenge: $foreign.setChallenge, 
    setCustomValidity: $foreign.setCustomValidity, 
    setDisabled: $foreign.setDisabled, 
    setKeytype: $foreign.setKeytype, 
    setName: $foreign.setName, 
    type_: $foreign.type_, 
    validationMessage: $foreign.validationMessage, 
    validity: $foreign.validity, 
    willValidate: $foreign.willValidate
};
