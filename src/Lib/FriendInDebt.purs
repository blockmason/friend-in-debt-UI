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
       , deleteFriendship
       , pendingFriends

       , newPendingDebt
       , confirmPendingDebt
       , rejectPendingDebt
       , debtBalances
       , itemizedDebts
       , pendingDebts

       , allNames
       , getCurrentUserName
       , setCurrentUserName
       ) where

--TODO: throw a different type of error if no FoundationId exists

import Prelude
import Network.Eth.FriendInDebt.Types
import Control.Monad.Eff           (Eff, kind Effect)
import Control.Monad.Eff.Console   (logShow, CONSOLE)
import Math                        (abs)
import Control.Monad.Eff.Class     (liftEff)
import Control.Monad.Aff.Class     (liftAff)
import Control.Monad.Aff           (Aff, makeAff)
import Control.Monad.Except.Trans  (ExceptT, throwError, runExceptT, lift)
import Data.Either                 (Either(Left, Right))
import Data.Maybe                  (Maybe(..), maybe)
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
import Network.Eth                                                 as E

infixr 9 compose as ∘

foreign import data FID ∷ Effect
type MonadF a = ∀ e. ExceptT Error (Aff (console ∷ CONSOLE, fid ∷ FID, metamask ∷ MM.METAMASK | e)) a
runMonadF = runExceptT

type PendingFriend = { friendId    ∷ StringId
                     , confirmerId ∷ StringId }

type IdLookupFn   = ∀ e. (StringId → Eff e Unit) → Eff e Unit
type BalanceLookupFn = ∀ e. (Array RawBalance → Eff e Unit) → StringId → Eff e Unit
type DebtLookupFn = ∀ e. (Array RawDebt → Eff e Unit) → StringId → Eff e Unit
type DebtLookupFn' = ∀ e. (Array RawConfirmed → Eff e Unit) → StringId → StringId → Eff e Unit
type NameLookupFn = ∀ e. (String → Eff e Unit) → StringId → Eff e Unit
type FriendsLookupFn = ∀ e. ((Array StringId) → Eff e Unit) → StringId → Eff e Unit
type PendingFriendsFn = ∀ e. ((Array PendingFriend) → Eff e Unit) → StringAddr → Eff e Unit
type ZeroArgTx = ∀ e. (E.RawTx → Eff e Unit)                   → Eff e Unit
type OneArgTx  = ∀ e. (E.RawTx → Eff e Unit) → String          → Eff e Unit
type TwoArgTx  = ∀ e. (E.RawTx → Eff e Unit) → String → String → Eff e Unit
type NewDebtTx  = ∀ e. (E.RawTx → Eff e Unit) → String → String → Number → String → Description → Eff e Unit
type HandleDebtTx = ∀ e. (E.RawTx → Eff e Unit) → StringId → StringId → Number → Eff e Unit

foreign import initImpl ∷ ∀ e. Unit → Eff e Unit
foreign import currentUserImpl ∷ ∀ e. Unit → Eff e StringAddr
foreign import getMyFoundationIdImpl ∷ IdLookupFn

foreign import friendsImpl ∷ FriendsLookupFn
foreign import pendingFriendshipsImpl ∷ PendingFriendsFn
foreign import createFriendshipImpl ∷ TwoArgTx
foreign import confirmFriendshipImpl ∷ TwoArgTx
foreign import deleteFriendshipImpl ∷ TwoArgTx

foreign import newPendingDebtImpl ∷ NewDebtTx
foreign import confirmDebtImpl  ∷ HandleDebtTx
foreign import rejectDebtImpl   ∷ HandleDebtTx
foreign import debtBalancesImpl ∷ BalanceLookupFn
foreign import pendingDebtsImpl ∷ DebtLookupFn
foreign import itemizedDebtsImpl ∷ DebtLookupFn'

foreign import getNameImpl ∷ NameLookupFn
foreign import setNameImpl ∷ ∀ e. String → Eff e Unit

--

checkAndInit ∷ MonadF Unit
checkAndInit = do
  loggedIn ← liftEff (MM.loggedIn <$> MM.checkStatus)
  if loggedIn
    then liftEff $ initImpl unit
    else throwError NoMetamask

currentUser ∷ MonadF E.EthAddress
currentUser = do
  checkAndInit
  E.eaMkAddr <$> (liftEff $ currentUserImpl unit)

foundationId ∷ MonadF FoundationId
foundationId = do
  checkAndInit
  fid ← liftAff $ makeAff (\err succ → getMyFoundationIdImpl succ)
  if fid == "" then throwError NoFoundationId else pure $ FoundationId fid

createFriendship ∷ FoundationId → MonadF E.TX
createFriendship (FoundationId newFriend) = do
  (FoundationId myId) ← foundationId
  tx ← liftAff $ makeAff (\e s → createFriendshipImpl s myId newFriend)
  E.rawToTX TxError tx

confirmFriendship ∷ FoundationId → MonadF E.TX
confirmFriendship (FoundationId newFriend) = do
  (FoundationId myId) ← foundationId
  tx ← liftAff $ makeAff (\_ s → confirmFriendshipImpl s myId newFriend)
  E.rawToTX TxError tx

deleteFriendship ∷ FoundationId → MonadF E.TX
deleteFriendship (FoundationId badFriend) = do
  (FoundationId myId) ← foundationId
  tx ← liftAff $ makeAff (\_ s → deleteFriendshipImpl s myId badFriend)
  E.rawToTX TxError tx

pendingFriends ∷ MonadF PendingFriendships
pendingFriends = do
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
  (FoundationId myId) ← foundationId
  friendList ← liftAff $ makeAff (\error success → friendsImpl success myId)
  pure $ FoundationId <$> friendList

newPendingDebt ∷ Debt → MonadF E.TX
newPendingDebt debt = do
  let m = debtMoney debt
      debtor = (fiGetId $ debtDebtor debt)
      creditor = (fiGetId $ debtCreditor debt)
  tx ← liftAff $ makeAff (\_ s → newPendingDebtImpl s debtor creditor (numAmount m) (show $ moneyCurrency m) (getDesc debt))
  E.rawToTX TxError tx

handleDebt ∷ HandleDebtTx → Debt → MonadF E.TX
handleDebt handleDebtFn debt = do
  (FoundationId myId) ← foundationId
  let debtId = debtGetId debt
      friendId = fiGetId $ debtCounterparty (fiMkId myId) debt
  if isValidDebtId $ debtId
    then do
      tx ← liftAff $ makeAff (\_ s → handleDebtFn s myId friendId (getDebtId debtId))
      E.rawToTX TxError tx
    else throwError InvalidDebtId

confirmPendingDebt ∷ Debt → MonadF E.TX
confirmPendingDebt = handleDebt confirmDebtImpl

rejectPendingDebt  ∷ Debt → MonadF E.TX
rejectPendingDebt  = handleDebt rejectDebtImpl

debtBalances ∷ MonadF (Array Balance)
debtBalances = do
  (FoundationId myId) ← foundationId
  rawBalances ← liftAff $ makeAff (\err succ → debtBalancesImpl succ myId)
  pure $ (rawToBalance (FoundationId myId)) <$> rawBalances

itemizedDebts ∷ FoundationId → MonadF (Array Debt)
itemizedDebts (FoundationId friendId) = do
  (FoundationId myId) ← foundationId
  rawDebts ← liftAff $ makeAff (\e s → itemizedDebtsImpl s myId friendId)
  pure $ rawConfirmedToDebt <$> rawDebts

pendingDebts ∷ MonadF PendingDebts
pendingDebts = do
  (FoundationId fi) ← foundationId
  debtList ← liftAff $ makeAff (\err succ → pendingDebtsImpl succ fi)
  let myId = FoundationId fi
      todo = (A.filter (\d → myId == (debtToConfirm d)) ∘ (map rawToDebt))
      sent = (A.filter (\d → myId /= (debtToConfirm d)) ∘ (map rawToDebt))
  pure $ PD { todo: todo debtList, sent: sent debtList }

{- FriendInDebtNS -}
getName ∷ ∀ e. FoundationId → Aff e (Maybe UserName)
getName (FoundationId fi) = do
  userName ← makeAff (\err succ → getNameImpl succ fi)
  if userName == "" then pure Nothing else pure $ Just userName

getCurrentUserName ∷ ∀ e. MonadF (Either FoundationId UserName)
getCurrentUserName = do
  myId ← foundationId
  (liftAff $ getName myId) >>= (pure ∘ (maybe (Left myId) Right))

setCurrentUserName ∷ ∀ e. UserName → MonadF Unit
setCurrentUserName userNameStr = do
  checkAndInit
  liftEff $ setNameImpl userNameStr

allNames ∷ ∀ e. Array FoundationId → MonadF (M.Map FoundationId UserName)
allNames friendList = do
  myId ← foundationId
  let allUsers = friendList <> [myId]
  names ← liftAff $ traverse getName $ allUsers
  pure $ foldr f M.empty $ A.zip allUsers names
  where f (Tuple address userName) map = insertMap map address userName
        insertMap map address Nothing         = map
        insertMap map address (Just userName) = M.insert address userName map
