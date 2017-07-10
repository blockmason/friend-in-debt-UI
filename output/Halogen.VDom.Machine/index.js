// Generated by purs version 0.11.5
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Functor = require("../Data.Functor");
var Data_Tuple = require("../Data.Tuple");
var Data_Unit = require("../Data.Unit");
var Data_Void = require("../Data.Void");
var Prelude = require("../Prelude");
var Step = (function () {
    function Step(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    Step.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new Step(value0, value1, value2);
            };
        };
    };
    return Step;
})();
var stepper = function (dictFunctor) {
    return function (f) {
        return function (h) {
            return function (a) {
                var next = function (b) {
                    return new Step(b, stepper(dictFunctor)(f)(h), h);
                };
                return Data_Functor.map(dictFunctor)(next)(f(a));
            };
        };
    };
};
var step = function (v) {
    return v.value1;
};
var never = function (dictApplicative) {
    return function (a) {
        return Control_Applicative.pure(dictApplicative)(new Step(Data_Void.absurd(a), never(dictApplicative), Control_Applicative.pure(dictApplicative)(Data_Unit.unit)));
    };
};
var loop = function (dictApplicative) {
    return function (f) {
        return function (g) {
            return function (s) {
                return function (a) {
                    var next = function (s$prime) {
                        return new Step(Data_Unit.unit, loop(dictApplicative)(f)(g)(s$prime), g(s$prime));
                    };
                    return Data_Functor.map((dictApplicative.Apply0()).Functor0())(next)(f(s)(a));
                };
            };
        };
    };
};
var halt = function (v) {
    return v.value2;
};
var functorStep = function (dictFunctor) {
    return new Data_Functor.Functor(function (f) {
        return function (v) {
            return new Step(f(v.value0), function ($32) {
                return Data_Functor.map(dictFunctor)(Data_Functor.map(functorStep(dictFunctor))(f))(v.value1($32));
            }, v.value2);
        };
    });
};
var fold = function (dictApplicative) {
    return function (f) {
        return function (g) {
            return function (s) {
                return function (a) {
                    var next = function (v) {
                        return new Step(v.value1, fold(dictApplicative)(f)(g)(v.value0), g(v.value0));
                    };
                    return Data_Functor.map((dictApplicative.Apply0()).Functor0())(next)(f(s)(a));
                };
            };
        };
    };
};
var extract = function (v) {
    return v.value0;
};
var constantly = function (dictApplicative) {
    return function (x) {
        return function (v) {
            return Control_Applicative.pure(dictApplicative)(new Step(x, constantly(dictApplicative)(x), Control_Applicative.pure(dictApplicative)(Data_Unit.unit)));
        };
    };
};
module.exports = {
    Step: Step, 
    constantly: constantly, 
    extract: extract, 
    fold: fold, 
    halt: halt, 
    loop: loop, 
    never: never, 
    step: step, 
    stepper: stepper, 
    functorStep: functorStep
};
