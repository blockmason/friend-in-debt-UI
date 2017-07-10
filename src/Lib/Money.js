"use strict";
/*   depends on accounting.js  */

exports.formatDollar = function(amount) {
    return accounting.formatMoney(amount);
};
