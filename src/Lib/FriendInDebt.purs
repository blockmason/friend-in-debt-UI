module Network.Eth.FriendInDebt
       (
         FID
       , currentUser
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
       , runMonadF
       , Error (..)
       , Money (..)
       , UserAddress (..)
       , UserName
       , BProgAddress
       , FriendDebt (..)
       , DebtCompare(..)
       ) where

import Prelude
import Control.Monad.Eff           (Eff, kind Effect)
import Math                        (abs)
import Control.Monad.Eff.Class     (liftEff)
import Control.Monad.Aff.Class     (liftAff)
import Control.Monad.Aff           (Aff, makeAff)
import Control.Monad.Except.Trans  (ExceptT, throwError, runExceptT, lift)
import Data.Either                 (Either(Left, Right))
import Data.Maybe                  (Maybe(..), fromMaybe)
import Data.Traversable            (traverse)
import Data.Format.Money           (formatDollar)
import Data.Int                    (toNumber)
import Data.String                 (localeCompare)
import Math                        (abs)
import Data.Map                                                    as M
import Data.Array                                                  as A
import Data.Tuple                  (Tuple(..))
import Data.Foldable               (foldr)
import Network.Eth.Metamask                                        as MM

infixr 9 compose as ∘

foreign import data FID ∷ Effect
type MonadF a = ∀ e. ExceptT Error (Aff (fid ∷ FID, metamask ∷ MM.METAMASK | e)) a
runMonadF = runExceptT

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

newtype FriendDebt = FriendDebt { friend     ∷ UserAddress
                                , debt       ∷ Money
                                , debtId     ∷ Int
                                , desc       ∷ String
                                , currency   ∷ String }
instance showFriendDebt ∷ Show FriendDebt where
  show (FriendDebt fd) = show fd.debt <> ": " <> show fd.friend
instance eqFriendDebt ∷ Eq FriendDebt where
  eq (FriendDebt fd1) (FriendDebt fd2) =
    (fd1.friend == fd2.friend) && (fd1.debt == fd2.debt)
blankFriendDebt ∷ FriendDebt
blankFriendDebt = FriendDebt { friend: UserAddress "0x0", debt: Money 0.0
                             , debtId: 0, desc: "", currency: "USDcents" }

friendDebtZero ∷ UserAddress → FriendDebt
friendDebtZero ua = changeDebtor ua blankFriendDebt
getDebt ∷ FriendDebt → Money
getDebt (FriendDebt fd) = fd.debt
setDebt ∷ FriendDebt → Number → FriendDebt
setDebt (FriendDebt fd) m = FriendDebt $ fd { debt = (Money m)}
getFriendAddr ∷ FriendDebt → UserAddress
getFriendAddr (FriendDebt fd) = fd.friend
changeDebtor ∷ UserAddress → FriendDebt → FriendDebt
changeDebtor newDebtor (FriendDebt fd) = FriendDebt $ fd {friend = newDebtor}
newDebt ∷ UserAddress → Number → FriendDebt
newDebt f d = FriendDebt { friend: f, debt: Money d, debtId: 0, desc: "", currency: "USDcents" }
addDebt :: FriendDebt -> Money -> FriendDebt
addDebt fd money = setDebt fd $ (amount (getDebt fd)) + (amount money)
--make a debt value negative
flipDebt ∷ FriendDebt → FriendDebt
flipDebt (FriendDebt fd) = FriendDebt $ fd { debt = mkNegative fd.debt }

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

checkAndInit ∷ MonadF Unit
checkAndInit = do
  loggedIn ← liftEff (MM.loggedIn <$> MM.checkStatus)
  if loggedIn
    then liftEff $ initImpl "dummy"
    else throwError NoMetamask

currentUser ∷ MonadF UserAddress
currentUser = do
  checkAndInit
  UserAddress <$> (liftEff $ currentUserImpl "dummy")

friends ∷ ∀ e. UserAddress → Aff e (Array UserAddress)
friends (UserAddress ua) = do
  friendList ← makeAff (\error success → friendsImpl success ua)
  pure $ UserAddress <$> friendList

getDebtOrPending ∷ ∀ e. DebtLookupFn → UserAddress → UserAddress → Aff e FriendDebt
getDebtOrPending lookupFnImpl (UserAddress d) (UserAddress c) = do
  m ← makeAff (\err succ → lookupFnImpl succ d c)
  pure $ newDebt (UserAddress c) m

allDebtOrPending ∷ ∀ e. DebtLookupFn → UserAddress → Array UserAddress
                 → Aff e (Array FriendDebt)
allDebtOrPending lookupFn debtor creditors =
  traverse (getDebtOrPending lookupFn debtor) creditors

currentUserFriends ∷ MonadF (Array UserAddress)
currentUserFriends = do
  cu ← currentUser
  liftAff $ friends cu

currentUserDebts ∷ Array UserAddress → MonadF (Array FriendDebt)
currentUserDebts friendList = do
  cu ← currentUser
  liftAff $ allDebtOrPending friendDebtImpl cu friendList

currentUserPending ∷ Array UserAddress → MonadF (Array FriendDebt)
currentUserPending friendList = do
  cu ← currentUser
  liftAff $ allDebtOrPending friendPendingImpl cu friendList

currentUserSentPendings ∷ Array UserAddress → MonadF (Array FriendDebt)
currentUserSentPendings friendList = do
  cu ← currentUser
  liftAff $ traverse
    (\f → (flipDebt ∘ (changeDebtor f)) <$> getDebtOrPending friendPendingImpl f cu)
    friendList

newPending ∷ FriendDebt → MonadF Unit
newPending (FriendDebt debtor) = do
  (UserAddress user) ← currentUser
  liftEff $ newPendingImpl (getUa debtor.friend) (amount debtor.debt)

confirmPending ∷ FriendDebt → MonadF Unit
confirmPending (FriendDebt fd) = do
  cu ← currentUser
  liftEff $ confirmPendingImpl (getUa fd.friend) (amount fd.debt)

cancelPending ∷ ∀ e. UserAddress → MonadF Unit
cancelPending (UserAddress user) = do
  checkAndInit
  liftEff $ cancelPendingImpl user

createFriendship ∷ ∀ e. UserAddress → MonadF Unit
createFriendship (UserAddress newFriend) = do
   checkAndInit
   liftEff $  createFriendshipImpl newFriend

getName ∷ ∀ e. UserAddress → Aff e (Maybe UserName)
getName (UserAddress ua) = do
  userName ← makeAff (\err succ → getNameImpl succ ua)
  if userName == "" then pure Nothing else pure $ Just userName

getCurrentUserName ∷ ∀ e. MonadF (Either UserAddress UserName)
getCurrentUserName = do
  cu ← currentUser
  maybeName ← liftAff $ getName cu
  case maybeName of
    Nothing → pure $ Left cu
    Just n  → pure $ Right n

setCurrentUserName ∷ ∀ e. UserName → MonadF Unit
setCurrentUserName userNameStr = do
  checkAndInit
  liftEff $ setNameImpl userNameStr

allNames ∷ ∀ e. Array UserAddress → MonadF (M.Map UserAddress UserName)
allNames friendList = do
  cu ← currentUser
  let allUsers = friendList <> [cu]
  names ← liftAff $ traverse getName $ allUsers
  pure $ foldr f M.empty $ A.zip allUsers names
  where f (Tuple address userName) map = insertMap map address userName
        insertMap map address Nothing         = map
        insertMap map address (Just userName) = M.insert address userName map


{-
to modify:
currentUserDebts
-qu
currentUserPending
currentUserSentPendings
currentUserFriends
get/set UserName

newPending
confirmPending
cancelPending

to add:
-pendingFriendships
-description fetching


-}
