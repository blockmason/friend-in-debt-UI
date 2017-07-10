// Generated by purs version 0.11.5
"use strict";
var Control_Monad_State_Class = require("../Control.Monad.State.Class");
var Control_Monad_State_Trans = require("../Control.Monad.State.Trans");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Identity = require("../Data.Identity");
var Data_Newtype = require("../Data.Newtype");
var Data_Tuple = require("../Data.Tuple");
var Prelude = require("../Prelude");
var withState = Control_Monad_State_Trans.withStateT;
var runState = function (v) {
    return function ($16) {
        return Data_Newtype.unwrap(Data_Identity.newtypeIdentity)(v($16));
    };
};
var mapState = function (f) {
    return Control_Monad_State_Trans.mapStateT(function ($17) {
        return Data_Identity.Identity(f(Data_Newtype.unwrap(Data_Identity.newtypeIdentity)($17)));
    });
};
var execState = function (v) {
    return function (s) {
        var v1 = v(s);
        return v1.value1;
    };
};
var evalState = function (v) {
    return function (s) {
        var v1 = v(s);
        return v1.value0;
    };
};
module.exports = {
    evalState: evalState, 
    execState: execState, 
    mapState: mapState, 
    runState: runState, 
    withState: withState
};
