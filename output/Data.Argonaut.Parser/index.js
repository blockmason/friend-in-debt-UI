// Generated by purs version 0.11.5
"use strict";
var $foreign = require("./foreign");
var Data_Argonaut_Core = require("../Data.Argonaut.Core");
var Data_Either = require("../Data.Either");
var Data_Function_Uncurried = require("../Data.Function.Uncurried");
var jsonParser = function (j) {
    return $foreign._jsonParser(Data_Either.Left.create, Data_Either.Right.create, j);
};
module.exports = {
    jsonParser: jsonParser
};
