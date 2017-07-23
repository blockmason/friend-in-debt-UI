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

infixr 9 compose as ∘

foreign import data FID ∷ Effect
type MonadF a = ∀ e. ExceptT Error (Aff (console ∷ CONSOLE, fid ∷ FID, metamask ∷ MM.METAMASK | e)) a
runMonadF = runExceptT

type PendingFriend = { friendId    ∷ StringId
                     , confirmerId ∷ StringId }

type IdLookupFn   = ∀ e. (StringId → Eff e Unit) → Eff e Unit
type BalanceLookupFn = ∀ e. (Array RawBalance → Eff e Unit) → StringId → Eff e Unit
type HandleDebtFn = ∀ e. StringId → StringId → Number → Eff e Unit
type DebtLookupFn = ∀ e. (Array RawDebt → Eff e Unit) → StringId → Eff e Unit
type NameLookupFn = ∀ e. (String → Eff e Unit) → StringId → Eff e Unit
type FriendsLookupFn = ∀ e. ((Array StringId) → Eff e Unit) → StringId → Eff e Unit
type PendingFriendsFn = ∀ e. ((Array PendingFriend) → Eff e Unit) → StringAddr → Eff e Unit

foreign import initImpl ∷ ∀ e. Unit → Eff e Unit
foreign import currentUserImpl ∷ ∀ e. Unit → Eff e StringAddr
foreign import getMyFoundationIdImpl ∷ IdLookupFn

foreign import friendsImpl ∷ FriendsLookupFn
foreign import pendingFriendshipsImpl ∷ PendingFriendsFn
foreign import createFriendshipImpl ∷ ∀ e. StringId → StringId → Eff e Unit
foreign import confirmFriendshipImpl ∷ ∀ e. StringId → StringId → Eff e Unit
foreign import deleteFriendshipImpl ∷ ∀ e. StringId → StringId → Eff e Unit

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

createFriendship ∷ FoundationId → MonadF Unit
createFriendship (FoundationId newFriend) = do
  (FoundationId myId) ← foundationId
  liftEff $ createFriendshipImpl myId newFriend

confirmFriendship ∷ FoundationId → MonadF Unit
confirmFriendship (FoundationId newFriend) = do
  (FoundationId myId) ← foundationId
  liftEff $ confirmFriendshipImpl myId newFriend

deleteFriendship ∷ FoundationId → MonadF Unit
deleteFriendship (FoundationId badFriend) = do
  (FoundationId myId) ← foundationId
  liftEff $ deleteFriendshipImpl myId badFriend

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

newPendingDebt ∷ Debt → MonadF Unit
newPendingDebt debt = do
  let m = debtMoney debt
      debtor = (fiGetId $ debtDebtor debt)
      creditor = (fiGetId $ debtCreditor debt)
  liftEff $ newPendingDebtImpl debtor creditor (numAmount m) (show $ moneyCurrency m) (getDesc debt)

handleDebt ∷ HandleDebtFn → Debt → MonadF Unit
handleDebt handleDebtFn debt = do
  (FoundationId myId) ← foundationId
  let debtId = debtGetId debt
      friendId = fiGetId $ debtCounterparty (fiMkId myId) debt
  if isValidDebtId $ debtId
    then liftEff $ handleDebtFn myId friendId (getDebtId debtId)
    else throwError InvalidDebtId

confirmPendingDebt ∷ Debt → MonadF Unit
confirmPendingDebt = handleDebt confirmDebtImpl

rejectPendingDebt  ∷ Debt → MonadF Unit
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
