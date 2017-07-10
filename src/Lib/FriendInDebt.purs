module Network.Eth.FriendInDebt
       (
         currentUser
       , currentUserDebts
       , currentUserPending
       , currentUserSentPendings
       , currentUserFriends
       , newPending
       , confirmPending
       , cancelPending
       , createFriendship
       , allNames
       , getCurrentUserName
       , setCurrentUserName
       , getName --DELETE
       , getDebt
       , amount
       , absMoney
       , setDebt
       , posNegZero
       , getFriendAddr
       , getUa
       , newDebt
       , flipDebt
       , blankFriendDebt
       , friendDebtZero
       , addDebt
       , Error (..)
       , Money (..)
       , UserAddress (..)
       , UserName
       , BProgAddress
       , FriendDebt (..)
       , DebtCompare(..)
       ) where

import Prelude
import Control.Monad.Eff         (Eff)
import Math                      (abs)
import Control.Monad.Eff.Class   (liftEff)
import Control.Monad.Aff         (Aff, makeAff)
import Data.Either               (Either(Left, Right))
import Data.Maybe                (Maybe(..))
import Data.Traversable          (traverse)
import Data.Format.Money         (formatDollar)
import Data.Int                  (toNumber)
import Data.String               (localeCompare)
import Math                      (abs)
import Data.Map                                                    as M
import Data.Array                                                  as A
import Data.Tuple                (Tuple(..))
import Data.Foldable             (foldr)
import Network.Eth.Metamask                                        as MM

infixr 9 compose as ∘

type BProgAddress = String
type DummyVal = String
type UserName = String

data Error = NoMetamask
instance showError ∷ Show Error where
  show NoMetamask = "NoMetamask: Metamask not logged in."

newtype Money = Money Number
instance showMoney ∷ Show Money where
  show (Money val) =
    (if (val < (toNumber 0)) then "-" else "") <> (formatDollar $ abs val)
instance eqMoney ∷ Eq Money where
  eq (Money m1) (Money m2) = m1 == m2
instance ordMoney ∷ Ord Money where
  compare (Money m1) (Money m2) = compare m1 m2
amount ∷ Money → Number
amount (Money m) = m
absMoney ∷ Money → Money
absMoney (Money m) = Money $ abs m
mkNegative ∷ Money → Money
mkNegative (Money m) = Money $ m * (-1.0)

newtype FriendDebt = FriendDebt { friend ∷ UserAddress
                                , debt   ∷ Money }
instance showFriendDebt ∷ Show FriendDebt where
  show (FriendDebt fd) = show fd.debt <> ": " <> show fd.friend
instance eqFriendDebt ∷ Eq FriendDebt where
  eq (FriendDebt fd1) (FriendDebt fd2) =
    (fd1.friend == fd2.friend) && (fd1.debt == fd2.debt)
blankFriendDebt ∷ FriendDebt
blankFriendDebt = (FriendDebt { friend: UserAddress "0x0", debt: Money 0.0 })
friendDebtZero ∷ UserAddress → FriendDebt
friendDebtZero ua = FriendDebt { friend: ua, debt: (Money 0.0) }
getDebt ∷ FriendDebt → Money
getDebt (FriendDebt fd) = fd.debt
setDebt ∷ FriendDebt → Number → FriendDebt
setDebt (FriendDebt fd) m = FriendDebt {friend: fd.friend, debt: (Money m)}
getFriendAddr ∷ FriendDebt → UserAddress
getFriendAddr (FriendDebt fd) = fd.friend
changeDebtor ∷ UserAddress → FriendDebt → FriendDebt
changeDebtor newDebtor (FriendDebt fd) = FriendDebt {friend: newDebtor, debt: fd.debt}
newDebt ∷ UserAddress → Number → FriendDebt
newDebt f d = FriendDebt { friend: f, debt: Money d }
addDebt :: FriendDebt -> Money -> FriendDebt
addDebt fd money = setDebt fd $ (amount (getDebt fd)) + (amount money)
--make a debt value negative
flipDebt ∷ FriendDebt → FriendDebt
flipDebt (FriendDebt fd) = FriendDebt { friend: fd.friend, debt: mkNegative fd.debt}

data DebtCompare = Positive | Negative | Zero
posNegZero ∷ FriendDebt → DebtCompare
posNegZero (FriendDebt fd) | amount fd.debt > 0.0 = Positive
                           | amount fd.debt < 0.0 = Negative
                           | otherwise            = Zero

newtype UserAddress = UserAddress BProgAddress
instance showUserAddress ∷ Show UserAddress where
  show (UserAddress ua) = ua
instance eqUserAddress ∷ Eq UserAddress where
  eq (UserAddress ua1) (UserAddress ua2) = ua1 == ua2
instance ordUserAddress ∷ Ord UserAddress where
  compare (UserAddress ua1) (UserAddress ua2) = localeCompare ua1 ua2
getUa ∷ UserAddress → String
getUa (UserAddress ua) = ua

type DebtLookupFn = ∀ e. (Number → Eff e Unit)
                       → BProgAddress → BProgAddress
                       → Eff e Unit
type NameLookupFn = ∀ e. (String → Eff e Unit) → BProgAddress → Eff e Unit

foreign import friendsImpl ∷ ∀ e. ((Array String) → Eff e Unit) → BProgAddress
                           → Eff e Unit
foreign import friendDebtImpl ∷ DebtLookupFn
foreign import friendPendingImpl ∷ DebtLookupFn
foreign import initImpl ∷ ∀ e. DummyVal → Eff e Unit
foreign import currentUserImpl ∷ ∀ e. DummyVal → Eff e String
foreign import newPendingImpl ∷ ∀ e. BProgAddress → Number → Eff e Unit
foreign import confirmPendingImpl ∷ ∀ e. BProgAddress → Number → Eff e Unit
foreign import cancelPendingImpl ∷ ∀ e. BProgAddress → Eff e Unit
foreign import createFriendshipImpl ∷ ∀ e. BProgAddress → Eff e Unit
foreign import getNameImpl ∷ NameLookupFn
foreign import setNameImpl ∷ ∀ e. String → Eff e Unit

checkAndInit ∷ ∀ e. Eff e (Either Error Unit)
checkAndInit = do
  loggedIn ← MM.loggedIn <$> MM.checkStatus
  if loggedIn
    then Right <$> (initImpl "dummy")
    else pure $ Left NoMetamask

currentUser ∷ ∀ e. Eff e (Either Error UserAddress)
currentUser = do
  c ← checkAndInit
  case c of
    Left error → pure $ Left error
    Right _ → do
      res ← currentUserImpl "dummy"
      pure $ Right $ UserAddress res

friends ∷ ∀ e. UserAddress → Aff e (Array UserAddress)
friends (UserAddress ua) = do
  friendList ← makeAff (\error success → friendsImpl success ua)
  pure $ UserAddress <$> friendList

getDebtOrPending ∷ ∀ e. DebtLookupFn → UserAddress → UserAddress → Aff e FriendDebt
getDebtOrPending lookupFnImpl (UserAddress d) (UserAddress c) = do
  m ← makeAff (\err succ → lookupFnImpl succ d c)
  pure $ FriendDebt $ { friend: (UserAddress c), debt: Money m }

allDebtOrPending ∷ ∀ e. DebtLookupFn → UserAddress → Array UserAddress
                 → Aff e (Array FriendDebt)
allDebtOrPending lookupFn debtor creditors =
  traverse (getDebtOrPending lookupFn debtor) creditors

currentUserFriends ∷ ∀ e. Aff e (Either Error (Array UserAddress))
currentUserFriends = do
  currUser ← liftEff currentUser
  case currUser of
    Left  error → pure $ Left error
    Right user  → Right <$> friends user

currentUserDebts ∷ ∀ e. Array UserAddress → Aff e (Either Error (Array FriendDebt))
currentUserDebts friendList = do
  currUser ← liftEff currentUser
  case currUser of
    Left  error → pure $ Left error
    Right user  → Right <$> allDebtOrPending friendDebtImpl user friendList

currentUserPending ∷ ∀ e. Array UserAddress → Aff e (Either Error (Array FriendDebt))
currentUserPending friendList = do
  u ← liftEff currentUser
  case u of
    Right user  → Right <$> allDebtOrPending friendPendingImpl user friendList
    Left  error → pure $ Left error

currentUserSentPendings ∷ ∀ e. Array UserAddress → Aff e (Either Error (Array FriendDebt))
currentUserSentPendings friendList = do
  u ← liftEff currentUser
  case u of
    Left error → pure $ Left error
    Right user → Right <$> do
      traverse (\f → (flipDebt ∘ (changeDebtor f)) <$> getDebtOrPending friendPendingImpl f user) friendList

newPending ∷ ∀ e. FriendDebt → Eff e (Either Error Unit)
newPending (FriendDebt debtor) = do
  cu ← currentUser
  case cu of
    Right (UserAddress user) →
      Right <$> newPendingImpl (getUa debtor.friend) (amount debtor.debt)
    Left  error → pure $ Left error

confirmPending ∷ ∀ e. FriendDebt → Eff e (Either Error Unit)
confirmPending (FriendDebt fd) = do
  cu ← currentUser
  case cu of
    Right (UserAddress user) →
      Right <$> confirmPendingImpl (getUa fd.friend) (amount fd.debt)
    Left  error → pure $ Left error

cancelPending ∷ ∀ e. UserAddress → Eff e (Either Error Unit)
cancelPending (UserAddress user) = do
  c ← checkAndInit
  case c of
    Left error → pure $ Left error
    Right _ → do
      cancelPendingImpl user
      pure $ Right unit

createFriendship ∷ ∀ e. UserAddress → Eff e (Either Error Unit)
createFriendship (UserAddress newFriend) = do
  c ← checkAndInit
  case c of
    Left error → pure $ Left error
    Right _ → do
      createFriendshipImpl newFriend
      pure $ Right unit

getName ∷ ∀ e. UserAddress → Aff e (Maybe UserName)
getName (UserAddress ua) = do
  userName ← makeAff (\err succ → getNameImpl succ ua)
  if userName == "" then pure Nothing else pure $ Just userName

getCurrentUserName ∷ ∀ e. Aff e (Either Error (Either UserAddress UserName))
getCurrentUserName = do
  cu ← liftEff currentUser
  case cu of
    Left error → pure $ Left error
    Right user → do
      maybeName ← getName user
      case maybeName of
        Nothing → pure $ Right $ Left  user
        Just n  → pure $ Right $ Right n

setCurrentUserName ∷ ∀ e. UserName → Eff e (Either Error Unit)
setCurrentUserName userNameStr = do
  c ← liftEff checkAndInit
  case c of
    Left error → pure $ Left error
    Right _    → Right <$> setNameImpl userNameStr

allNames ∷ ∀ e. Array UserAddress → Aff e (Either Error (M.Map UserAddress UserName))
allNames friendList = do
  cu ← liftEff currentUser
  case cu of
    Left error → pure $ Left error
    Right user → do
      let allUsers = friendList <> [user]
      names ← traverse getName $ allUsers
      pure $ Right <$> foldr f M.empty $ A.zip allUsers names
  where f (Tuple address userName) map = insertMap map address userName
        insertMap map address Nothing         = map
        insertMap map address (Just userName) = M.insert address userName map
