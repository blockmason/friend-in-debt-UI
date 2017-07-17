"use strict";
//requires web3, FriendInDebt, FriendInDebtNS configs

var FriendInDebt;
var FriendInDebtNS;

exports.initImpl = function(dummyVal) {
    return function() {
        FriendInDebt = TruffleContract(friendInDebtConfig);
        FriendInDebt.setProvider(web3.currentProvider);
        FriendInDebtNS = TruffleContract(friendInDebtNSConfig);
        FriendInDebtNS.setProvider(web3.currentProvider);
    };
};

exports.friendsImpl = function(callback) {
    return function(userAddress) {
        return function() {
            FriendInDebt.deployed().then(function(instance) {
                return instance.getFriends.call(userAddress);
            }).then(function(res) {
                callback(res.valueOf())();
            });
        };
    };
};

exports.pendingFriendshipsImpl = function(callback) {
    return function(userAddress) {
        return function() {
            FriendInDebt.deployed().then(function(instance) {
                return instance.pendingFriends.call(userAddress);
            }).then(function(res) {
                callback(pendingFriends2Js(res.valueOf()))();
            });
        };
    };
};

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
            callback(res.valueOf())();
        });
    };
};

exports.newPendingImpl = function(creditor) {
    return function(amount) {
        return function() {
            FriendInDebt.deployed().then(function(instance) {
                return instance.newPending(creditor, amount);
            });
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


exports.createFriendshipImpl = function(friend) {
    return function() {
        FriendInDebt.deployed().then(function(instance) {
            return instance.createFriendship(friend);
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
    var friendList = [];
    for ( var i=0; i < friends.length; i++ ) {
        var friend = {
            friendId: b2s(friends[i])
        };
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
                     creditor: b2s(debts[2][i]) };
        balanceList.push(debt);
    }
    return balanceList;
};
