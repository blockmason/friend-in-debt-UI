// Generated by purs version 0.11.5
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Plus = require("../Control.Plus");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Either = require("../Data.Either");
var Data_Eq = require("../Data.Eq");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Interval_Duration = require("../Data.Interval.Duration");
var Data_List = require("../Data.List");
var Data_List_NonEmpty = require("../Data.List.NonEmpty");
var Data_List_Types = require("../Data.List.Types");
var Data_Map = require("../Data.Map");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_Monoid_Additive = require("../Data.Monoid.Additive");
var Data_Newtype = require("../Data.Newtype");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Data_Tuple = require("../Data.Tuple");
var $$Math = require("../Math");
var Prelude = require("../Prelude");
var IsoDuration = function (x) {
    return x;
};
var IsEmpty = (function () {
    function IsEmpty() {

    };
    IsEmpty.value = new IsEmpty();
    return IsEmpty;
})();
var InvalidWeekComponentUsage = (function () {
    function InvalidWeekComponentUsage() {

    };
    InvalidWeekComponentUsage.value = new InvalidWeekComponentUsage();
    return InvalidWeekComponentUsage;
})();
var ContainsNegativeValue = (function () {
    function ContainsNegativeValue(value0) {
        this.value0 = value0;
    };
    ContainsNegativeValue.create = function (value0) {
        return new ContainsNegativeValue(value0);
    };
    return ContainsNegativeValue;
})();
var InvalidFractionalUse = (function () {
    function InvalidFractionalUse(value0) {
        this.value0 = value0;
    };
    InvalidFractionalUse.create = function (value0) {
        return new InvalidFractionalUse(value0);
    };
    return InvalidFractionalUse;
})();
var unIsoDuration = function (v) {
    return v;
};
var showIsoDuration = new Data_Show.Show(function (v) {
    return "(IsoDuration " + (Data_Show.show(Data_Interval_Duration.showDuration)(v) + ")");
});
var showError = new Data_Show.Show(function (v) {
    if (v instanceof IsEmpty) {
        return "(IsEmpty)";
    };
    if (v instanceof InvalidWeekComponentUsage) {
        return "(InvalidWeekComponentUsage)";
    };
    if (v instanceof ContainsNegativeValue) {
        return "(ContainsNegativeValue " + (Data_Show.show(Data_Interval_Duration.showDurationComponent)(v.value0) + ")");
    };
    if (v instanceof InvalidFractionalUse) {
        return "(InvalidFractionalUse " + (Data_Show.show(Data_Interval_Duration.showDurationComponent)(v.value0) + ")");
    };
    throw new Error("Failed pattern match at Data.Interval.Duration.Iso line 44, column 3 - line 45, column 3: " + [ v.constructor.name ]);
});
var prettyError = function (v) {
    if (v instanceof IsEmpty) {
        return "Duration is empty (has no components)";
    };
    if (v instanceof InvalidWeekComponentUsage) {
        return "Week component of Duration is used with other components";
    };
    if (v instanceof ContainsNegativeValue) {
        return "Component `" + (Data_Show.show(Data_Interval_Duration.showDurationComponent)(v.value0) + "` contains negative value");
    };
    if (v instanceof InvalidFractionalUse) {
        return "Invalid usage of Fractional value at component `" + (Data_Show.show(Data_Interval_Duration.showDurationComponent)(v.value0) + "`");
    };
    throw new Error("Failed pattern match at Data.Interval.Duration.Iso line 50, column 1 - line 51, column 1: " + [ v.constructor.name ]);
};
var eqIsoDuration = new Data_Eq.Eq(function (x) {
    return function (y) {
        return Data_Eq.eq(Data_Interval_Duration.eqDuration)(x)(y);
    };
});
var ordIsoDuration = new Data_Ord.Ord(function () {
    return eqIsoDuration;
}, function (x) {
    return function (y) {
        return Data_Ord.compare(Data_Interval_Duration.ordDuration)(x)(y);
    };
});
var eqError = new Data_Eq.Eq(function (x) {
    return function (y) {
        if (x instanceof IsEmpty && y instanceof IsEmpty) {
            return true;
        };
        if (x instanceof InvalidWeekComponentUsage && y instanceof InvalidWeekComponentUsage) {
            return true;
        };
        if (x instanceof ContainsNegativeValue && y instanceof ContainsNegativeValue) {
            return Data_Eq.eq(Data_Interval_Duration.eqDurationComponent)(x.value0)(y.value0);
        };
        if (x instanceof InvalidFractionalUse && y instanceof InvalidFractionalUse) {
            return Data_Eq.eq(Data_Interval_Duration.eqDurationComponent)(x.value0)(y.value0);
        };
        return false;
    };
});
var ordError = new Data_Ord.Ord(function () {
    return eqError;
}, function (x) {
    return function (y) {
        if (x instanceof IsEmpty && y instanceof IsEmpty) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof IsEmpty) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof IsEmpty) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof InvalidWeekComponentUsage && y instanceof InvalidWeekComponentUsage) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof InvalidWeekComponentUsage) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof InvalidWeekComponentUsage) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof ContainsNegativeValue && y instanceof ContainsNegativeValue) {
            return Data_Ord.compare(Data_Interval_Duration.ordDurationComponent)(x.value0)(y.value0);
        };
        if (x instanceof ContainsNegativeValue) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof ContainsNegativeValue) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof InvalidFractionalUse && y instanceof InvalidFractionalUse) {
            return Data_Ord.compare(Data_Interval_Duration.ordDurationComponent)(x.value0)(y.value0);
        };
        throw new Error("Failed pattern match at Data.Interval.Duration.Iso line 42, column 1 - line 42, column 38: " + [ x.constructor.name, y.constructor.name ]);
    };
});
var checkWeekUsage = function (v) {
    var $60 = Data_Maybe.isJust(Data_Map.lookup(Data_Interval_Duration.ordDurationComponent)(Data_Interval_Duration.Week.value)(v.asMap)) && Data_Map.size(v.asMap) > 1;
    if ($60) {
        return Control_Applicative.pure(Data_List_Types.applicativeList)(InvalidWeekComponentUsage.value);
    };
    return Control_Plus.empty(Data_List_Types.plusList);
};
var checkNegativeValues = function (v) {
    return Data_Function.flip(Data_Foldable.foldMap(Data_List_Types.foldableList)(Data_List_Types.monoidList))(v.asList)(function (v1) {
        var $64 = v1.value1 >= 0.0;
        if ($64) {
            return Control_Plus.empty(Data_List_Types.plusList);
        };
        return Control_Applicative.pure(Data_List_Types.applicativeList)(new ContainsNegativeValue(v1.value0));
    });
};
var checkFractionalUse = function (v) {
    var isFractional = function (a) {
        return $$Math.floor(a) !== a;
    };
    var checkRest = function (rest) {
        return Data_Newtype.unwrap(Data_Monoid_Additive.newtypeAdditive)(Data_Foldable.foldMap(Data_List_Types.foldableList)(Data_Monoid_Additive.monoidAdditive(Data_Semiring.semiringNumber))(function ($81) {
            return Data_Monoid_Additive.Additive($$Math.abs(Data_Tuple.snd($81)));
        })(rest)) > 0.0;
    };
    var v1 = (function (v2) {
        return v2.rest;
    })(Data_List.span(function ($82) {
        return Data_HeytingAlgebra.not(Data_HeytingAlgebra.heytingAlgebraFunction(Data_HeytingAlgebra.heytingAlgebraBoolean))(isFractional)(Data_Tuple.snd($82));
    })(v.asList));
    if (v1 instanceof Data_List_Types.Cons && checkRest(v1.value1)) {
        return Control_Applicative.pure(Data_List_Types.applicativeList)(new InvalidFractionalUse(v1.value0.value0));
    };
    return Control_Plus.empty(Data_List_Types.plusList);
};
var checkEmptiness = function (v) {
    var $76 = Data_List["null"](v.asList);
    if ($76) {
        return Control_Applicative.pure(Data_List_Types.applicativeList)(IsEmpty.value);
    };
    return Control_Plus.empty(Data_List_Types.plusList);
};
var checkValidIsoDuration = function (v) {
    var check = Data_Foldable.fold(Data_Foldable.foldableArray)(Data_Monoid.monoidFn(Data_List_Types.monoidList))([ checkWeekUsage, checkEmptiness, checkFractionalUse, checkNegativeValues ]);
    var asList = Data_List.reverse(Data_Map.toAscUnfoldable(Data_List_Types.unfoldableList)(v));
    return check({
        asList: asList, 
        asMap: v
    });
};
var mkIsoDuration = function (d) {
    var v = Data_List_NonEmpty.fromList(checkValidIsoDuration(d));
    if (v instanceof Data_Maybe.Just) {
        return new Data_Either.Left(v.value0);
    };
    if (v instanceof Data_Maybe.Nothing) {
        return new Data_Either.Right(d);
    };
    throw new Error("Failed pattern match at Data.Interval.Duration.Iso line 60, column 19 - line 62, column 35: " + [ v.constructor.name ]);
};
module.exports = {
    IsEmpty: IsEmpty, 
    InvalidWeekComponentUsage: InvalidWeekComponentUsage, 
    ContainsNegativeValue: ContainsNegativeValue, 
    InvalidFractionalUse: InvalidFractionalUse, 
    mkIsoDuration: mkIsoDuration, 
    prettyError: prettyError, 
    unIsoDuration: unIsoDuration, 
    eqIsoDuration: eqIsoDuration, 
    ordIsoDuration: ordIsoDuration, 
    showIsoDuration: showIsoDuration, 
    eqError: eqError, 
    ordError: ordError, 
    showError: showError
};
