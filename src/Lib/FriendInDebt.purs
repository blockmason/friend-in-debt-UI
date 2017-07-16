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
       , EthAddress (..)
       , UserName
       , StringAddr
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
import Network.Eth.FoundationId    (FoundationId(..))

infixr 9 compose as ∘

foreign import data FID ∷ Effect
type MonadF a = ∀ e. ExceptT Error (Aff (fid ∷ FID, metamask ∷ MM.METAMASK | e)) a
runMonadF = runExceptT

type StringAddr = String
type StringId   = String
type DummyVal   = String
type UserName   = String

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

newtype FriendDebt = FriendDebt { friend     ∷ EthAddress
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
blankFriendDebt = FriendDebt { friend: EthAddress "0x0", debt: Money 0.0
                             , debtId: 0, desc: "", currency: "USDcents" }

friendDebtZero ∷ EthAddress → FriendDebt
friendDebtZero ua = changeDebtor ua blankFriendDebt
getDebt ∷ FriendDebt → Money
getDebt (FriendDebt fd) = fd.debt
setDebt ∷ FriendDebt → Number → FriendDebt
setDebt (FriendDebt fd) m = FriendDebt $ fd { debt = (Money m)}
getFriendAddr ∷ FriendDebt → EthAddress
getFriendAddr (FriendDebt fd) = fd.friend
changeDebtor ∷ EthAddress → FriendDebt → FriendDebt
changeDebtor newDebtor (FriendDebt fd) = FriendDebt $ fd {friend = newDebtor}
newDebt ∷ EthAddress → Number → FriendDebt
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

newtype EthAddress = EthAddress StringAddr
instance showEthAddress ∷ Show EthAddress where
  show (EthAddress ua) = ua
instance eqEthAddress ∷ Eq EthAddress where
  eq (EthAddress ua1) (EthAddress ua2) = ua1 == ua2
instance ordEthAddress ∷ Ord EthAddress where
  compare (EthAddress ua1) (EthAddress ua2) = localeCompare ua1 ua2
getUa ∷ EthAddress → String
getUa (EthAddress ua) = ua

type PendingFriend = { friendId    ∷ StringId
                     , confirmerId ∷ StringId }

type IdLookupFn   = ∀ e. (StringId → Eff e Unit) → Eff e Unit
type DebtLookupFn = ∀ e. (Number → Eff e Unit)
                       → StringAddr → StringAddr
                       → Eff e Unit
type NameLookupFn = ∀ e. (String → Eff e Unit) → StringAddr → Eff e Unit
type FriendsLookupFn = ∀ e. ((Array String) → Eff e Unit) → StringAddr → Eff e Unit
type PendingFriendsFn = ∀ e. ((Array PendingFriend) → Eff e Unit) → StringAddr → Eff e Unit

foreign import initImpl ∷ ∀ e. Unit → Eff e Unit
foreign import currentUserImpl ∷ ∀ e. Unit → Eff e StringAddr
foreign import getMyFoundationIdImpl ∷ IdLookupFn
foreign import friendsImpl ∷ FriendsLookupFn
foreign import friendDebtImpl ∷ DebtLookupFn
foreign import friendPendingImpl ∷ DebtLookupFn
foreign import pendingFriendshipsImpl ∷ PendingFriendsFn
foreign import newPendingImpl ∷ ∀ e. StringAddr → Number → Eff e Unit
foreign import confirmPendingImpl ∷ ∀ e. StringAddr → Number → Eff e Unit
foreign import cancelPendingImpl ∷ ∀ e. StringAddr → Eff e Unit
foreign import createFriendshipImpl ∷ ∀ e. StringAddr → Eff e Unit
foreign import getNameImpl ∷ NameLookupFn
foreign import setNameImpl ∷ ∀ e. String → Eff e Unit

checkAndInit ∷ MonadF Unit
checkAndInit = do
  loggedIn ← liftEff (MM.loggedIn <$> MM.checkStatus)
  if loggedIn
    then liftEff $ initImpl unit
    else throwError NoMetamask

currentUser ∷ MonadF EthAddress
currentUser = do
  checkAndInit
  EthAddress <$> (liftEff $ currentUserImpl unit)

foundationId ∷ MonadF FoundationId
foundationId = do
  checkAndInit
  liftAff $ FoundationId <$> (makeAff (\err succ → getMyFoundationIdImpl succ))

friends ∷ ∀ e. EthAddress → Aff e (Array EthAddress)
friends (EthAddress ua) = do
  friendList ← makeAff (\error success → friendsImpl success ua)
  pure $ EthAddress <$> friendList

getDebtOrPending ∷ ∀ e. DebtLookupFn → EthAddress → EthAddress → Aff e FriendDebt
getDebtOrPending lookupFnImpl (EthAddress d) (EthAddress c) = do
  m ← makeAff (\err succ → lookupFnImpl succ d c)
  pure $ newDebt (EthAddress c) m

allDebtOrPending ∷ ∀ e. DebtLookupFn → EthAddress → Array EthAddress
                 → Aff e (Array FriendDebt)
allDebtOrPending lookupFn debtor creditors =
  traverse (getDebtOrPending lookupFn debtor) creditors

currentUserFriends ∷ MonadF (Array EthAddress)
currentUserFriends = do
  cu ← currentUser
  liftAff $ friends cu

currentUserDebts ∷ Array EthAddress → MonadF (Array FriendDebt)
currentUserDebts friendList = do
  cu ← currentUser
  liftAff $ allDebtOrPending friendDebtImpl cu friendList

currentUserPending ∷ Array EthAddress → MonadF (Array FriendDebt)
currentUserPending friendList = do
  cu ← currentUser
  liftAff $ allDebtOrPending friendPendingImpl cu friendList

currentUserSentPendings ∷ Array EthAddress → MonadF (Array FriendDebt)
currentUserSentPendings friendList = do
  cu ← currentUser
  liftAff $ traverse
    (\f → (flipDebt ∘ (changeDebtor f)) <$> getDebtOrPending friendPendingImpl f cu)
    friendList

pendingFriendsSent ∷ MonadF (Array FoundationId)
pendingFriendsSent = do
  (FoundationId fi) ← foundationId
  friendList ← liftAff $ makeAff (\err succ → pendingFriendshipsImpl succ fi)
  pure $ A.catMaybes $ (g (FoundationId fi)) <$> friendList
    where g myId pending = if (FoundationId pending.confirmerId) /= myId
                           then Just (FoundationId pending.friendId)
                           else Nothing

pendingFriendsTodo ∷ MonadF (Array FoundationId)
pendingFriendsTodo = do
  (FoundationId fi) ← foundationId
  friendList ← liftAff $ makeAff (\err succ → pendingFriendshipsImpl succ fi)
  pure $ A.catMaybes $ (g (FoundationId fi)) <$> friendList
    where g myId pending = if (FoundationId pending.confirmerId) == myId
                           then Just (FoundationId pending.friendId)
                           else Nothing

newPending ∷ FriendDebt → MonadF Unit
newPending (FriendDebt debtor) = do
  (EthAddress user) ← currentUser
  liftEff $ newPendingImpl (getUa debtor.friend) (amount debtor.debt)

confirmPending ∷ FriendDebt → MonadF Unit
confirmPending (FriendDebt fd) = do
  cu ← currentUser
  liftEff $ confirmPendingImpl (getUa fd.friend) (amount fd.debt)

cancelPending ∷ ∀ e. EthAddress → MonadF Unit
cancelPending (EthAddress user) = do
  checkAndInit
  liftEff $ cancelPendingImpl user

createFriendship ∷ ∀ e. EthAddress → MonadF Unit
createFriendship (EthAddress newFriend) = do
   checkAndInit
   liftEff $  createFriendshipImpl newFriend

getName ∷ ∀ e. EthAddress → Aff e (Maybe UserName)
getName (EthAddress ua) = do
  userName ← makeAff (\err succ → getNameImpl succ ua)
  if userName == "" then pure Nothing else pure $ Just userName

getCurrentUserName ∷ ∀ e. MonadF (Either EthAddress UserName)
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

allNames ∷ ∀ e. Array EthAddress → MonadF (M.Map EthAddress UserName)
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
-
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
