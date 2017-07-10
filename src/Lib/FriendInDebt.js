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
