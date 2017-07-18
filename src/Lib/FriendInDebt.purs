module Network.Eth.FriendInDebt
       (
         FID
       , foundationId
       , confirmedFriends
       , createFriendship
       , currentUserDebts
       , currentUserPending
       , currentUserSentPendings
       , pendingFriendsSent
       , pendingFriendsTodo
       , newPending
       , confirmPending
       , cancelPending
       , createFriendship
       , allNames
       , getCurrentUserName
       , setCurrentUserName
       , runMonadF
       , module Network.Eth.FriendInDebt.Types
       , module Network.Eth.Foundation
       ) where

import Prelude
import Network.Eth.FriendInDebt.Types
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
import Network.Eth.Foundation

infixr 9 compose as ∘

foreign import data FID ∷ Effect
type MonadF a = ∀ e. ExceptT Error (Aff (fid ∷ FID, metamask ∷ MM.METAMASK | e)) a
runMonadF = runExceptT

type PendingFriend = { friendId    ∷ StringId
                     , confirmerId ∷ StringId }

type IdLookupFn   = ∀ e. (StringId → Eff e Unit) → Eff e Unit
type DebtLookupFn = ∀ e. (Number → Eff e Unit)
                       → StringAddr → StringAddr
                       → Eff e Unit
type NameLookupFn = ∀ e. (String → Eff e Unit) → StringAddr → Eff e Unit
type FriendsLookupFn = ∀ e. ((Array StringId) → Eff e Unit) → StringId → Eff e Unit
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
foreign import createFriendshipImpl ∷ ∀ e. StringId → StringId → Eff e Unit
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

friends ∷ ∀ e. FoundationId → Aff e (Array FoundationId)
friends (FoundationId fi) = do
  friendList ← makeAff (\error success → friendsImpl success fi)
  pure $ FoundationId <$> friendList

getDebtOrPending ∷ ∀ e. DebtLookupFn → EthAddress → EthAddress → Aff e FriendDebt
getDebtOrPending lookupFnImpl (EthAddress d) (EthAddress c) = do
  m ← makeAff (\err succ → lookupFnImpl succ d c)
  pure $ newDebt (EthAddress c) m

allDebtOrPending ∷ ∀ e. DebtLookupFn → EthAddress → Array EthAddress
                 → Aff e (Array FriendDebt)
allDebtOrPending lookupFn debtor creditors =
  traverse (getDebtOrPending lookupFn debtor) creditors

createFriendship ∷ ∀ e. FoundationId → MonadF Unit
createFriendship (FoundationId newFriend) = do
  (FoundationId fi) ← foundationId
  liftEff $ createFriendshipImpl fi newFriend

confirmedFriends ∷ MonadF (Array FoundationId)
confirmedFriends = do
  fi ← foundationId
  liftAff $ friends fi

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

pendingFriends ∷ ∀ a. (FoundationId → FoundationId → Boolean)
                 → MonadF (Array FoundationId)
pendingFriends compFn = do
  (FoundationId fi) ← foundationId
  friendList ← liftAff $ makeAff (\err succ → pendingFriendshipsImpl succ fi)
  pure $ A.catMaybes $ (g (FoundationId fi)) <$> friendList
    where g myId pending = if compFn (FoundationId pending.confirmerId) myId
                           then Just (FoundationId pending.friendId)
                           else Nothing

pendingFriendsSent ∷ MonadF (Array FoundationId)
pendingFriendsSent = pendingFriends (/=)

pendingFriendsTodo ∷ MonadF (Array FoundationId)
pendingFriendsTodo = pendingFriends (==)

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
-description fetching
-confirm debts by debtId

-}
