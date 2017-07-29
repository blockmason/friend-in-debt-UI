"use strict";
//requires web3, Debt, Friend configs

var Debt;
var Friend;

var debtAbi;
var debtContract;
var debtContractAddress;

var friendAbi;
var friendContract;
var friendContractAddress;

var myAddress;

exports.initImpl = function(dummyVal) {
    return function() {
        debtAbi = web3.eth.contract(debtConfig.abi);
        debtContract = debtAbi.at(debtConfig.address);
        debtContractAddress = debtConfig.address;

        friendAbi = web3.eth.contract(friendConfig.abi);
        friendContract = friendAbi.at(friendConfig.address);
        friendContractAddress = friendConfig.address;

        Debt = TruffleContract(debtConfig);
        Debt.setProvider(web3.currentProvider);
        Friend = TruffleContract(friendConfig);
        Friend.setProvider(web3.currentProvider);
    };
};


exports.currentUserImpl = function(dummyVal) {
    return function() {
        return web3.eth.accounts[0];
    };
};

exports.getMyFoundationIdImpl = function(callback) {
    return function() {
        Debt.deployed().then(function(instance) {
            return instance.getMyFoundationId.call();
        }).then(function(res) {
            callback(b2s(res.valueOf()))();
        });
    };
};

/* Friend Functions */
exports.friendsImpl = function(callback) {
    return function(foundationId) {
        return function() {
            Friend.deployed().then(function(instance) {
                return instance.confirmedFriends.call(foundationId);
            }).then(function(res) {
                callback(confirmedFriends2Js(res.valueOf()))();
            });
        };
    };
};

exports.pendingFriendshipsImpl = function(callback) {
    return function(foundationId) {
        return function() {
            Friend.deployed().then(function(instance) {
                return instance.pendingFriends.call(foundationId);
            }).then(function(res) {
                callback(pendingFriends2Js(res.valueOf()))();
            });
        };
    };
};

exports.createFriendshipImpl = function(callback) {
    return function(myId) {
        return function(friendId) {
            return function() {
                var data = friendContract.addFriend.getData(myId, friendId);
                sendFriendTx(data, 0, callback);
            };
        };
    };
};

exports.confirmFriendshipImpl = function(callback) {
    return function(myId) {
        return function(friendId) {
            return function() {
                var data = friendContract.addFriend.getData(myId, friendId);
                sendFriendTx(data, 0, callback);
            };
        };
    };
};

exports.deleteFriendshipImpl = function(callback) {
    return function(myId) {
        return function(friendId) {
            return function() {
                var data = friendContract.deleteFriend.getData(myId, friendId);
                sendFriendTx(data, 0, callback);
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
                            var data = debtContract.newDebt(debtor, creditor, currencyCode, amount, desc);
                            sendDebtTx(data, 0, callback);
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
            Debt.deployed().then(function(instance) {
                return instance.confirmedDebtBalances.call(foundationId);
            }).then(function(res) {
                callback(debtBalances2Js(res.valueOf()))();
            });
        };
    };
};

exports.pendingDebtsImpl = function(callback) {
    return function(foundationId) {
        return function() {
            Debt.deployed().then(function(instance) {
                return instance.pendingDebts.call(foundationId);
            }).then(function(res) {
                callback(pendingDebts2Js(res.valueOf()))();
            });
        };
    };
};

exports.itemizedDebtsImpl = function(callback) {
    return function(myId) {
        return function(friendId) {
            return function() {
                Debt.deployed().then(function(instance) {
                    return instance.confirmedDebts.call(myId, friendId);
                }).then(function(res) {
                    callback(confirmedDebts2Js(res.valueOf()))();
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
                    var data = debtContract.confirmDebt(debtor, creditor, debtId);
                    sendDebtTx(data, 0, callback);
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
                    var data = debtContract.rejectDebt(debtor, creditor, debtId);
                    sendDebtTx(data, 0, callback);
                };
            };
        };
    };
};

//helper functions
var sendDebtTx = function(data, value, callback) {
    web3.eth.sendTransaction(
        {to: debtContractAddress,
         from: myAddress,
         data: data,
         value: value},
        function(err, result) {
            if ( !err )
                callback(goodTx(result))();
            else
                callback(errTx())();
        });
};

var sendFriendTx = function(data, value, callback) {
    web3.eth.sendTransaction(
        {to: friendContractAddress,
         from: myAddress,
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
