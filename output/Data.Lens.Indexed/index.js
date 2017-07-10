// Generated by purs version 0.11.5
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Category = require("../Control.Category");
var Control_Monad_State = require("../Control.Monad.State");
var Control_Monad_State_Class = require("../Control.Monad.State.Class");
var Control_Monad_State_Trans = require("../Control.Monad.State.Trans");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Functor_Compose = require("../Data.Functor.Compose");
var Data_Identity = require("../Data.Identity");
var Data_Lens_Internal_Indexed = require("../Data.Lens.Internal.Indexed");
var Data_Lens_Internal_Wander = require("../Data.Lens.Internal.Wander");
var Data_Lens_Types = require("../Data.Lens.Types");
var Data_Newtype = require("../Data.Newtype");
var Data_Profunctor = require("../Data.Profunctor");
var Data_Profunctor_Star = require("../Data.Profunctor.Star");
var Data_Semiring = require("../Data.Semiring");
var Data_Tuple = require("../Data.Tuple");
var Prelude = require("../Prelude");
var unIndex = function (dictProfunctor) {
    return function (l) {
        return function ($7) {
            return l(Data_Lens_Internal_Indexed.Indexed(Data_Profunctor.dimap(dictProfunctor)(Data_Tuple.snd)(Control_Category.id(Control_Category.categoryFn))($7)));
        };
    };
};
var iwander = function (itr) {
    return function (dictWander) {
        return function ($8) {
            return Data_Lens_Internal_Wander.wander(dictWander)(function (dictApplicative) {
                return function (f) {
                    return function (s) {
                        return itr(dictApplicative)(Data_Tuple.curry(f))(s);
                    };
                };
            })(Data_Newtype.unwrap(Data_Lens_Internal_Indexed.newtypeIndexed)($8));
        };
    };
};
var positions = function (tr) {
    return function (dictWander) {
        return iwander(function (dictApplicative) {
            return function (f) {
                return function (s) {
                    return Data_Function.flip(Control_Monad_State.evalState)(0)(Data_Newtype.unwrap(Data_Functor_Compose.newtypeCompose)(Data_Function.flip(Data_Newtype.unwrap(Data_Profunctor_Star.newtypeStar))(s)(tr(Data_Lens_Internal_Wander.wanderStar(Data_Functor_Compose.applicativeCompose(Control_Monad_State_Trans.applicativeStateT(Data_Identity.monadIdentity))(dictApplicative)))(function (a) {
                        return Data_Functor_Compose.Compose(Control_Apply.applyFirst(Control_Monad_State_Trans.applyStateT(Data_Identity.monadIdentity))(Control_Apply.apply(Control_Monad_State_Trans.applyStateT(Data_Identity.monadIdentity))(Data_Functor.map(Control_Monad_State_Trans.functorStateT(Data_Identity.functorIdentity))(f)(Control_Monad_State_Class.get(Control_Monad_State_Trans.monadStateStateT(Data_Identity.monadIdentity))))(Control_Applicative.pure(Control_Monad_State_Trans.applicativeStateT(Data_Identity.monadIdentity))(a)))(Control_Monad_State_Class.modify(Control_Monad_State_Trans.monadStateStateT(Data_Identity.monadIdentity))(function (v) {
                            return v + 1 | 0;
                        })));
                    }))));
                };
            };
        })(dictWander);
    };
};
var asIndex = function (dictProfunctor) {
    return function (l) {
        return function ($9) {
            return l(Data_Lens_Internal_Indexed.Indexed(Data_Profunctor.dimap(dictProfunctor)(Data_Tuple.fst)(Control_Category.id(Control_Category.categoryFn))($9)));
        };
    };
};
module.exports = {
    asIndex: asIndex, 
    iwander: iwander, 
    positions: positions, 
    unIndex: unIndex
};
