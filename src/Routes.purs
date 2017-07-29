module FriendInDebt.Routes where

import FriendInDebt.Prelude

data Screen =
    BalancesScreen
  | FriendsScreen
  | PendingScreen
  | SettingsScreen
  | AddFriendScreen
  | CreateDebtScreen
  | ItemizedDebtsScreen

instance eqScreen ∷ Eq Screen where
  eq screen1 screen2 = (getRouteNameFor screen1) == (getRouteNameFor screen2)

getBaseName ∷ Screen → String
getBaseName screen =
  case screen of
    BalancesScreen →
      "balances-screen"
    FriendsScreen →
      "friends-screen"
    PendingScreen →
      "pending-screen"
    SettingsScreen →
      "settings-screen"
    AddFriendScreen →
      "add-friend-screen"
    CreateDebtScreen →
      "create-debt-screen"
    ItemizedDebtsScreen →
      "itemized-debts-screen"

getRouteNameFor ∷ Screen → String
getRouteNameFor = (append "show-") ∘ getBaseName

getContainerNameFor ∷ Screen → String
getContainerNameFor = (\x → append x "-container") ∘ getBaseName

getMenuNameFor ∷ Screen → String
getMenuNameFor screen =
  case screen of
    BalancesScreen →
      "Balances"
    FriendsScreen →
      "Friends"
    PendingScreen →
      "Pending"
    SettingsScreen →
      "Settings"
    _ →
      ""
