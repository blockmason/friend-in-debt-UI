"use strict";
//requires web3, Debt, Friend configs

var foundContract;
var foundAddress;

var debtReader;
var debtReaderAddress;

var friendReader;
var friendReaderAddress;

var flux;
var fluxAddress;

var fidUcac;

//var myAddress;

exports.initImpl = function(dummyVal) {
    return function() {
        debtReader = web3.eth.contract(debtReaderConfig.abi).at(debtReaderConfig.address);
        debtReaderAddress = debtReaderConfig.address;

        friendReader = web3.eth.contract(friendReaderConfig.abi).at(friendReaderConfig.address);
        friendReaderAddress = friendReaderConfig.address;

        flux = web3.eth.contract(fluxConfig.abi).at(fluxConfig.address);
        fluxAddress = fluxConfig.address;

        foundContract = web3.eth.contract(foundationConfig.abi).at(foundationConfig.address);
        foundAddress = foundationConfig.address;

        fidUcac = fidConfig.address;
    };
};

exports.nameInUseImpl = function(callback) {
    return function(foundationId) {
        return function() {
            foundContract.resolveToAddresses(foundationId, function(e, r) {
                if (!e) callback(r.valueOf().length > 0)();
                else    console.error(e);
            });
        };
    };
};

exports.getMyFoundationIdImpl = function(callback) {
    return function(addr) {
        return function() {
            foundContract.resolveToName(addr, function(e, r) {
                if (!e) callback(b2s(r.valueOf()))();
                else {
                    console.error(e);
                    callback("ERR")();
                }
            });
        };
    };
};

/* Friend Functions */
exports.friendsImpl = function(callback) {
    return function(foundationId) {
        return function() {
            friendReader.confirmedFriends(fidUcac, foundationId, function(e,r) {
                callback(confirmedFriends2Js(r.valueOf()))();
            });
        };
    };
};

exports.pendingFriendshipsImpl = function(callback) {
    return function(foundationId) {
        return function() {
            friendReader.pendingFriends(fidUcac, foundationId, function(e,r) {
                callback(pendingFriends2Js(r.valueOf()))();
            });
        };
    };
};

exports.createFriendshipImpl = function(callback) {
    return function(myId) {
        return function(friendId) {
            return function() {
                var data = flux.addFriend.getData(fidUcac, myId, friendId);
                sendFluxTx(data, 0, callback);
            };
        };
    };
};

exports.confirmFriendshipImpl = function(callback) {
    return function(myId) {
        return function(friendId) {
            return function() {
                var data = flux.addFriend.getData(fidUcac, myId, friendId);
                sendFluxTx(data, 0, callback);
            };
        };
    };
};

exports.deleteFriendshipImpl = function(callback) {
    return function(myId) {
        return function(friendId) {
            return function() {
                var data = flux.deleteFriend.getData(fidUcac, myId, friendId);
                sendFluxTx(data, 0, callback);
            };
        };
    };
};


/* Debt Functions */
exports.newPendingDebtImpl = function(callback) {
    return function(debtor) {
        return function(creditor) {
            return function(amount) {
                return function(currencyCode) {
                    return function(desc) {
                        return function() {
                            var data = flux.newDebt.getData(fidUcac, debtor, creditor, currencyCode, amount, desc);
                            sendFluxTx(data, 0, callback);
                        };
                    };
                };
            };
        };
    };
};

exports.debtBalancesImpl = function(callback) {
    return function(foundationId) {
        return function() {
            debtReader.confirmedDebtBalances(fidUcac, foundationId, function(e,r) {
                callback(debtBalances2Js(r.valueOf()))();
            });
        };
    };
};

exports.pendingDebtsImpl = function(callback) {
    return function(foundationId) {
        return function() {
            debtReader.pendingDebts(fidUcac, foundationId, function(e,r) {
                callback(pendingDebts2Js(r.valueOf()))();
            });
        };
    };
};

exports.itemizedDebtsImpl = function(callback) {
    return function(myId) {
        return function(friendId) {
            return function() {
                debtReader.confirmedDebts(fidUcac, myId, friendId, function(e,r) {
                    callback(confirmedDebts2Js(r.valueOf()))();
                });
            };
        };
    };
};

exports.confirmDebtImpl = function(callback) {
    return function(myId) {
        return function(friendId) {
            return function(debtId) {
                return function() {
                    var data = flux.confirmDebt.getData(fidUcac, myId, friendId, debtId);
                    sendFluxTx(data, 0, callback);
                };
            };
        };
    };
};

exports.rejectDebtImpl = function(callback) {
    return function(myId) {
        return function(friendId) {
            return function(debtId) {
                return function() {
                    var data = flux.rejectDebt.getData(fidUcac, myId, friendId, debtId);
                    sendFluxTx(data, 0, callback);
                };
            };
        };
    };
};

//helpers
var sendFluxTx = function(data, value, callback) {
    web3.eth.sendTransaction(
        {to: fluxAddress,
         data: data,
         value: value},
        function(err, result) {
            if ( !err )
                callback(goodTx(result))();
            else
                callback(errTx())();
        });
};

var goodTx = function(t) {
    return { txHash: t, error: false };
};

var errTx = function() {
    return { txHash: "", error: true };
};



var confirmedFriends2Js = function(friends) {
    var friendList = [];
    for ( var i=0; i < friends.length; i++ ) {
        var friend = b2s(friends[i]);
        friendList.push(friend);
    }
    return friendList;
};

var pendingFriends2Js = function(friends) {
    var friendList = [];
    for ( var i=0; i < friends[0].length; i++ ) {
        var friend = {
            friendId: b2s(friends[0][i]),
            confirmerId: b2s(friends[1][i])
        };
        friendList.push(friend);
    }
    return friendList;
};

var pendingDebts2Js = function(debts) {
    var debtList = [];
    //debts[0] is all the debtIds, debts[1] is confirmerIds, etc
    for ( var i=0; i < debts[0].length; i++ ) {
        var debt = { id: debts[0][i].toNumber(),
                     confirmerId: b2s(debts[1][i]),
                     currency: b2s(debts[2][i]),
                     amount: debts[3][i].toNumber(),
                     desc: b2s(debts[4][i]),
                     debtor: b2s(debts[5][i]),
                     creditor: b2s(debts[6][i]),
                     timestamp: 0.0 };
        debtList.push(debt);
    }
    return debtList;
};

var confirmedDebts2Js = function(debts) {
    var debtList = [];
    for ( var i=0; i < debts[0].length; i++ ) {
        var debt = { currency: b2s(debts[0][i]),
                     amount: debts[1][i].toNumber(),
                     desc: b2s(debts[2][i]),
                     debtor: b2s(debts[3][i]),
                     creditor: b2s(debts[4][i]),
                     timestamp: debts[5][i].toNumber() };
        debtList.push(debt);
    }
    return debtList;
};

var debtBalances2Js = function(debts) {
    var balanceList = [];
    for ( var i=0; i < debts[0].length; i++ ) {
        var debt = { currency: b2s(debts[0][i]),
                     amount: debts[1][i].toNumber(),
                     counterParty: b2s(debts[2][i]),
                     totalDebts: debts[3][i].toNumber(),
                     mostRecent: debts[4][i].toNumber() };
        balanceList.push(debt);
    }
    return balanceList;
};

var b2s = function(bytes) {
    var s = "";
    for(var i=2; i<bytes.length; i+=2) {
        var num = parseInt(bytes.substring(i, i+2), 16);
        if (num == 0) break;
        var char = String.fromCharCode(num);
        s += char;
    }
    return s;

};
