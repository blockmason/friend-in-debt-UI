"use strict";
//requires web3, FriendInDebt, FriendInDebtNS, Friendships configs

var FriendInDebt;
var FriendInDebtNS;
var Friendships;

exports.initImpl = function(dummyVal) {
    return function() {
        FriendInDebt = TruffleContract(friendInDebtConfig);
        FriendInDebt.setProvider(web3.currentProvider);
        FriendInDebtNS = TruffleContract(friendInDebtNSConfig);
        FriendInDebtNS.setProvider(web3.currentProvider);
        Friendships = TruffleContract(friendshipsConfig);
        Friendships.setProvider(web3.currentProvider);
    };
};


exports.currentUserImpl = function(dummyVal) {
    return function() {
        return web3.eth.accounts[0];
    };
};

exports.getMyFoundationIdImpl = function(callback) {
    return function() {
        FriendInDebt.deployed().then(function(instance) {
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
            Friendships.deployed().then(function(instance) {
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
            Friendships.deployed().then(function(instance) {
                return instance.pendingFriends.call(foundationId);
            }).then(function(res) {
                callback(pendingFriends2Js(res.valueOf()))();
            });
        };
    };
};

exports.createFriendshipImpl = function(myId) {
    return function(friendId) {
        return function() {
            Friendships.deployed().then(function(instance) {
                return instance.addFriend(myId, friendId);
            });
        };
    };
};

exports.confirmFriendshipImpl = function(myId) {
    return function(friendId) {
        return function() {
            Friendships.deployed().then(function(instance) {
                return instance.addFriend(myId, friendId);
            });
        };
    };
};

/* Debt Functions */
exports.newPendingDebtImpl = function(debtor) {
    return function(creditor) {
        return function(amount) {
            return function(currencyCode) {
                return function(desc) {
                    return function() {
                        FriendInDebt.deployed().then(function(instance) {
                            return instance.newDebt(debtor, creditor, currencyCode, amount, desc);
                        });
                    };
                };
            };
        };
    };
};

exports.debtBalancesImpl = function(callback) {
    return function(foundationId) {
        return function() {
            FriendInDebt.deployed().then(function(instance) {
                return instance.confirmedDebtBalances.call(foundationId);
            }).then(function(res) {
                callback(debtBalances2Js(res.valueOf()))();
            });
        };
    };
};

/////////////////////////////////


exports.friendDebtImpl = function(callback) {
    return function(debtor) {
        return function(creditor) {
            return function() {
                FriendInDebt.deployed().then(function(instance) {
                    return instance.getBalance.call(debtor, creditor);
                }).then(function(res) {
                    callback(res.toNumber())();
                });
            };
        };
    };
};

exports.friendPendingImpl = function(callback) {
    return function(debtor) {
        return function(creditor) {
            return function() {
                FriendInDebt.deployed().then(function(instance) {
                    return instance.getPendingAmount.call(debtor, creditor);
                }).then(function(res) {
                    callback(res.toNumber())();
                });
            };
        };
    };
};

exports.confirmPendingImpl = function(creditor) {
    return function(amount) {
        return function() {
            FriendInDebt.deployed().then(function(instance) {
                return instance.confirmPending(creditor, amount);
            });
        };
    };
};

exports.cancelPendingImpl = function(creditor) {
    return function() {
        FriendInDebt.deployed().then(function(instance) {
            return instance.cancelPending(creditor);
        });
    };
};

exports.getNameImpl = function(callback) {
    return function(user) {
        return function() {
            FriendInDebtNS.deployed().then(function(instance) {
                return instance.getName.call(user);
            }).then(function(res) {
                callback(res.valueOf())();
            });
        };
    };
};

exports.setNameImpl = function(newName) {
    return function() {
        FriendInDebtNS.deployed().then(function(instance) {
            return instance.setName(newName);
        });
    };
};

//helper functions
var confirmedFriends2Js = function(friends) {
    for ( var i=0; i < friends.length; i++ ) {
        friends[i] = b2s(friends[i]);
    }
    return friends;
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
                     creditor: b2s(debts[6][i])  };
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
                     creditor: b2s(debts[4][i])  };
        debtList.push(debt);
    }
    return debtList;
};

var debtBalances2Js = function(debts) {
    var balanceList = [];
    for ( var i=0; i < debts[0].length; i++ ) {
        var debt = { currency: b2s(debts[0][i]),
                     amount: debts[1][i].toNumber(),
                     counterParty: b2s(debts[2][i]) };
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
