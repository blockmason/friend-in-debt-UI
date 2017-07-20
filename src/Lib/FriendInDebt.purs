module Network.Eth.FriendInDebt
       (
         FID
       , runMonadF
       , module Network.Eth.FriendInDebt.Types
       , module Network.Eth.Foundation

       , foundationId
       , confirmedFriends
       , createFriendship
       , confirmFriendship
       , pendingFriendships

       , newPendingDebt
       , confirmPendingDebt
       , rejectPendingDebt
       , debtBalances
       , pendingDebts

       , allNames
       , getCurrentUserName
       , setCurrentUserName
       ) where

--TODO: throw a different type of error if no FoundationId exists

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
type BalanceLookupFn = ∀ e. (Array RawBalance → Eff e Unit) → StringId → Eff e Unit
type HandleDebtFn = ∀ e. StringId → StringId → Number → Eff e Unit
type DebtLookupFn = ∀ e. (Array RawDebt → Eff e Unit) → StringId → Eff e Unit
type NameLookupFn = ∀ e. (String → Eff e Unit) → StringAddr → Eff e Unit
type FriendsLookupFn = ∀ e. ((Array StringId) → Eff e Unit) → StringId → Eff e Unit
type PendingFriendsFn = ∀ e. ((Array PendingFriend) → Eff e Unit) → StringAddr → Eff e Unit

foreign import initImpl ∷ ∀ e. Unit → Eff e Unit
foreign import currentUserImpl ∷ ∀ e. Unit → Eff e StringAddr
foreign import getMyFoundationIdImpl ∷ IdLookupFn

foreign import friendsImpl ∷ FriendsLookupFn
foreign import pendingFriendshipsImpl ∷ PendingFriendsFn
foreign import createFriendshipImpl ∷ ∀ e. StringId → StringId → Eff e Unit
foreign import confirmFriendshipImpl ∷ ∀ e. StringId → StringId → Eff e Unit

foreign import newPendingDebtImpl ∷ ∀ e. StringId → StringId → Number → String
                                  → Description → Eff e Unit
foreign import confirmDebtImpl  ∷ HandleDebtFn
foreign import rejectDebtImpl   ∷ HandleDebtFn
foreign import debtBalancesImpl ∷ BalanceLookupFn
foreign import pendingDebtsImpl ∷ DebtLookupFn

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
  fid ← liftAff $ makeAff (\err succ → getMyFoundationIdImpl succ)
  if fid == "" then throwError NoFoundationId else pure $ FoundationId fid

friends ∷ ∀ e. FoundationId → Aff e (Array FoundationId)
friends (FoundationId fi) = do
  friendList ← makeAff (\error success → friendsImpl success fi)
  pure $ FoundationId <$> friendList

createFriendship ∷ FoundationId → MonadF Unit
createFriendship (FoundationId newFriend) = do
  (FoundationId myId) ← foundationId
  liftEff $ createFriendshipImpl myId newFriend

confirmFriendship ∷ FoundationId → MonadF Unit
confirmFriendship (FoundationId newFriend) = do
  (FoundationId myId) ← foundationId
  liftEff $ confirmFriendshipImpl myId newFriend

pendingFriendships ∷ MonadF PendingFriendships
pendingFriendships = do
  (FoundationId fi) ← foundationId
  friendList ← liftAff $ makeAff (\err succ → pendingFriendshipsImpl succ fi)
  let myId = FoundationId fi
      friendList' = (\f → Tuple (FoundationId f.confirmerId) (FoundationId f.friendId))
        <$> friendList
      todo = A.filter (\(Tuple cid _) → myId == cid) friendList'
      sent = A.filter (\(Tuple cid _) → myId /= cid) friendList'
  pure $ PF { todo: (\(Tuple _ fid) → fid) <$> todo
            , sent: (\(Tuple _ fid) → fid) <$> sent }

confirmedFriends ∷ MonadF (Array FoundationId)
confirmedFriends = do
  myId ← foundationId
  liftAff $ friends myId

newPendingDebt ∷ FoundationId → FoundationId → Money → Description → MonadF Unit
newPendingDebt (FoundationId debtor) (FoundationId creditor) m desc = do
  liftEff $ newPendingDebtImpl debtor creditor (numAmount m) (strCurrency m) desc

handleDebt ∷ HandleDebtFn → FoundationId → DebtId → MonadF Unit
handleDebt handleDebtFn (FoundationId friendId) debtId = do
  (FoundationId myId) ← foundationId
  if isValidDebtId debtId
    then liftEff $ handleDebtFn myId friendId (getDebtId debtId)
    else throwError InvalidDebtId

confirmPendingDebt ∷ FoundationId → DebtId → MonadF Unit
confirmPendingDebt = handleDebt confirmDebtImpl

rejectPendingDebt  ∷ FoundationId → DebtId → MonadF Unit
rejectPendingDebt  = handleDebt rejectDebtImpl

debtBalances ∷ MonadF (Array Balance)
debtBalances = do
  (FoundationId myId) ← foundationId
  rawBalances ← liftAff $ makeAff (\err succ → debtBalancesImpl succ myId)
  pure $ (rawToBalance (FoundationId myId)) <$> rawBalances

pendingDebts ∷ MonadF PendingDebts
pendingDebts = do
  (FoundationId fi) ← foundationId
  debtList ← liftAff $ makeAff (\err succ → pendingDebtsImpl succ fi)
  let myId = FoundationId fi
      todo = (A.filter (\d → myId == (debtToConfirm d)) ∘ (map rawToDebt))
      sent = (A.filter (\d → myId /= (debtToConfirm d)) ∘ (map rawToDebt))
  pure $ PD { todo: todo debtList, sent: sent debtList }

--pendingDebtsSent ∷ MonadF (Array Debt)

--pendingDebtsTodo ∷ MonadF (Array Debt)


{- FriendInDebtNS -}
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
