var debtConfig = {
    "contract_name": "FriendInDebt",
    "abi": [{"constant":false,"inputs":[{"name":"myId","type":"bytes32"},{"name":"friendId","type":"bytes32"},{"name":"debtId","type":"uint256"}],"name":"confirmDebt","outputs":[],"payable":false,"type":"function"},{"constant":false,"inputs":[{"name":"debtorId","type":"bytes32"},{"name":"creditorId","type":"bytes32"},{"name":"currencyCode","type":"bytes32"},{"name":"amount","type":"int256"},{"name":"desc","type":"bytes32"}],"name":"newDebt","outputs":[],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"p1","type":"bytes32"},{"name":"p2","type":"bytes32"}],"name":"confirmedDebts","outputs":[{"name":"currency2","type":"bytes32[]"},{"name":"amounts2","type":"int256[]"},{"name":"descs2","type":"bytes32[]"},{"name":"debtors2","type":"bytes32[]"},{"name":"creditors2","type":"bytes32[]"},{"name":"timestamps2","type":"uint256[]"}],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"fId","type":"bytes32"}],"name":"pendingDebts","outputs":[{"name":"debtIds","type":"uint256[]"},{"name":"confirmerIds","type":"bytes32[]"},{"name":"currency","type":"bytes32[]"},{"name":"amounts","type":"int256[]"},{"name":"descs","type":"bytes32[]"},{"name":"debtors","type":"bytes32[]"},{"name":"creditors","type":"bytes32[]"}],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"_currencyCode","type":"bytes32"}],"name":"isActiveCurrency","outputs":[{"name":"","type":"bool"}],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"s","type":"bytes32"},{"name":"l","type":"bytes32[]"}],"name":"isMember","outputs":[{"name":"","type":"bool"}],"payable":false,"type":"function"},{"constant":false,"inputs":[{"name":"myId","type":"bytes32"},{"name":"friendId","type":"bytes32"},{"name":"debtId","type":"uint256"}],"name":"rejectDebt","outputs":[],"payable":false,"type":"function"},{"constant":true,"inputs":[],"name":"getMyFoundationId","outputs":[{"name":"foundationId","type":"bytes32"}],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"fId","type":"bytes32"}],"name":"pendingDebtTimestamps","outputs":[{"name":"timestamps","type":"uint256[]"}],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"fId","type":"bytes32"}],"name":"confirmedDebtBalances","outputs":[{"name":"currency","type":"bytes32[]"},{"name":"amounts","type":"int256[]"},{"name":"counterpartyIds","type":"bytes32[]"},{"name":"totalDebts","type":"uint256[]"},{"name":"mostRecent","type":"uint256[]"}],"payable":false,"type":"function"},{"constant":false,"inputs":[{"name":"_currencyCode","type":"bytes32"}],"name":"addCurrencyCode","outputs":[],"payable":false,"type":"function"},{"inputs":[{"name":"_adminId","type":"bytes32"},{"name":"dataContract","type":"address"},{"name":"friendContract","type":"address"},{"name":"foundationContract","type":"address"}],"payable":false,"type":"constructor"}],
    "unlinked_binary": "",
    "address": "0xdc7a8b966fdcb9f73c1cf39d8327c32b34420271",
    "network_id": 3
};

var friendConfig = {
    "contract_name": "Friendships",
    "abi": [{"constant":false,"inputs":[{"name":"fId","type":"bytes32"},{"name":"index","type":"uint256"}],"name":"friendIdByIndex","outputs":[{"name":"","type":"bytes32"}],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"fId","type":"bytes32"}],"name":"confirmedFriends","outputs":[{"name":"confirmedFriends","type":"bytes32[]"}],"payable":false,"type":"function"},{"constant":false,"inputs":[{"name":"myId","type":"bytes32"},{"name":"friendId","type":"bytes32"}],"name":"deleteFriend","outputs":[],"payable":false,"type":"function"},{"constant":false,"inputs":[{"name":"myId","type":"bytes32"},{"name":"friendId","type":"bytes32"}],"name":"addFriend","outputs":[],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"fId","type":"bytes32"}],"name":"pendingFriends","outputs":[{"name":"friendIds","type":"bytes32[]"},{"name":"confirmerIds","type":"bytes32[]"}],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"_id1","type":"bytes32"},{"name":"_id2","type":"bytes32"}],"name":"areFriends","outputs":[{"name":"","type":"bool"}],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"fId","type":"bytes32"}],"name":"numFriends","outputs":[{"name":"","type":"uint256"}],"payable":false,"type":"function"},{"inputs":[{"name":"dataContract","type":"address"},{"name":"foundationContract","type":"address"}],"payable":false,"type":"constructor"}],
    "unlinked_binary": "",
    "address": "0x2b2b8d79cd3c2b87473d0afd0ea44a98c7f12da2",
    "network_id": 3
};
