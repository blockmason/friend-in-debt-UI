"use strict";
/*   depends on accounting.js  */

exports.formatDollar = function(amount) {
    return accounting.formatMoney(amount);
};

exports.formatDecimal = function(amount) {
    return function(decimals) {
        return accounting.formatMoney(amount, "", decimals);
    };
};
