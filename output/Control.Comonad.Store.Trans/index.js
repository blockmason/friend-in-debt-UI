// Generated by purs version 0.11.5
"use strict";
var Control_Comonad = require("../Control.Comonad");
var Control_Comonad_Trans_Class = require("../Control.Comonad.Trans.Class");
var Control_Extend = require("../Control.Extend");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Newtype = require("../Data.Newtype");
var Data_Tuple = require("../Data.Tuple");
var Prelude = require("../Prelude");
var StoreT = function (x) {
    return x;
};
var runStoreT = function (v) {
    return v;
};
var newtypeStoreT = new Data_Newtype.Newtype(function (n) {
    return n;
}, StoreT);
var functorStoreT = function (dictFunctor) {
    return new Data_Functor.Functor(function (f) {
        return function (v) {
            return StoreT(new Data_Tuple.Tuple(Data_Functor.map(dictFunctor)(function (h) {
                return function ($28) {
                    return f(h($28));
                };
            })(v.value0), v.value1));
        };
    });
};
var extendStoreT = function (dictExtend) {
    return new Control_Extend.Extend(function () {
        return functorStoreT(dictExtend.Functor0());
    }, function (f) {
        return function (v) {
            return StoreT(new Data_Tuple.Tuple(Control_Extend.extend(dictExtend)(function (w$prime) {
                return function (s$prime) {
                    return f(StoreT(new Data_Tuple.Tuple(w$prime, s$prime)));
                };
            })(v.value0), v.value1));
        };
    });
};
var comonadTransStoreT = new Control_Comonad_Trans_Class.ComonadTrans(function (dictComonad) {
    return function (v) {
        return Data_Functor.map((dictComonad.Extend0()).Functor0())(function (v1) {
            return v1(v.value1);
        })(v.value0);
    };
});
var comonadStoreT = function (dictComonad) {
    return new Control_Comonad.Comonad(function () {
        return extendStoreT(dictComonad.Extend0());
    }, function (v) {
        return Control_Comonad.extract(dictComonad)(v.value0)(v.value1);
    });
};
module.exports = {
    StoreT: StoreT, 
    runStoreT: runStoreT, 
    newtypeStoreT: newtypeStoreT, 
    functorStoreT: functorStoreT, 
    extendStoreT: extendStoreT, 
    comonadStoreT: comonadStoreT, 
    comonadTransStoreT: comonadTransStoreT
};
