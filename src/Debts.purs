module Debts where

import FriendInDebt.Prelude
import Utils
import Types (FIDMonad, ContainerMsgBus, ContainerMsg(..), NameMap(..), DebtMap(..))
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Aff (Aff)
import Data.Array (singleton)
import Control.Monad.Aff.Bus as Bus
import Data.Int (toNumber, decimal, fromStringAs)
import Data.Number as N
import Data.String as S
import Data.Map    as M
import Data.Array (length, filter, zip, fromFoldable)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import Network.Eth.FriendInDebt as F

data Query a
  = RefreshDebts a
  | HandleInput Input a
  | CreateDebt F.FriendDebt a
  | SendDebt F.UserAddress a
  | ConfirmPending F.FriendDebt a
  | CancelPending F.FriendDebt a
  | AddFriend (Either String F.UserAddress) a
  | InputFriend String a
  | InputName String a
  | UpdateName String a

type State = { friends     ∷ Array F.UserAddress
             , debts       ∷ Array F.FriendDebt
             , pending     ∷ Array F.FriendDebt
             , sentPending ∷ Array F.FriendDebt
             , creating    ∷ DebtMap
             , names       ∷ NameMap
             , newFriend   ∷ Either String F.UserAddress
             , userName    ∷ Either F.UserAddress F.UserName
             , inputName   ∷ String
             , loading     ∷ Boolean
             , errorBus    ∷ ContainerMsgBus }

type Input = ContainerMsgBus

component ∷ ∀ eff. H.Component HH.HTML Query Input Void (FIDMonad eff)
component =
  H.component
    { initialState: initialState
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where

  initialState ∷ Input → State
  initialState input = { friends: []
                       , debts: []
                       , pending: []
                       , sentPending: []
                       , creating: M.empty
                       , names:    M.empty
                       , newFriend: Left ""
                       , userName: (Right "")
                       , inputName: ""
                       , loading: false
                       , errorBus: input }

  render ∷ State → H.ComponentHTML Query
  render state =
    if state.loading
    then HH.span_ [ HH.h6_ [ HH.text "Loading debt info..." ]
                  , HH.img [ HP.src "loading.gif"
                           , HP.width 25 ] ]
    else
      HH.div_
      [
        -- HH.div
        -- [ HP.class_ $ HH.ClassName "refresh-button-container" ]
        -- [ refreshButton ],
      HH.div
        [ HP.class_ $ HH.ClassName "confirm-debts-container" ]
        [ HH.ul_ $ (displayPending state.names) <$> pending]
      , HH.h1_ [ HH.text "Current Debts"]
      , HH.div
        [ HP.class_ $ HH.ClassName "current-debts-container" ]
        [
          HH.ul_ $ (displayAllDebtLi state.names) <$>
          (zip state.debts state.sentPending)
        ]
      , HH.div
        [ HP.class_ $ HH.ClassName "all-friends-container" ]
        [
          HH.ul_ $ displayFriendLi <$> friendNames
        ]
      , HH.div
        [ HP.class_ $ HH.ClassName "add-friend-name-change-container" ]
        [
          HH.div
            [ HP.class_ $ HH.ClassName "add-friend-container" ]
            [
              addFriendWidget state
            ]
          , HH.div
            [ HP.class_ $ HH.ClassName "name-change-container" ]
            [
              nameChangeWidget state.inputName state.userName
            ]
        ]
      , HH.div
        [ HP.class_ $ HH.ClassName "create-debt-container" ]
        [
          HH.h5_ [ HH.text "Create Debt" ]
        , HH.ul_ $ (\friend → HH.li_ [ createDebt state.names state.creating friend]) <$> state.friends
        ]
      ]
    where pending = filter (\(Tuple _ fd2) -> nonZero fd2) $ zip state.debts state.pending
          friendNames = fromFoldable $ M.values state.names

  eval ∷ Query ~> H.ComponentDSL State Query Void (FIDMonad eff)
  eval = case _ of
    HandleInput input next → do
      H.modify (_ { errorBus = input })
      pure next
    AddFriend eitherFriend next → do
      s ← H.get
      case eitherFriend of
        Left str → pure next
        Right ua → do H.modify (_ { newFriend = Left "" })
                      handleFIDCallEff s.errorBus unit (F.createFriendship ua)
                      pure next
    InputFriend friendStr next → do
      if ((S.length friendStr) == 42)
        then H.modify (_ { newFriend = Right $ F.UserAddress friendStr })
        else H.modify (_ { newFriend = Left friendStr })
      pure next
    InputName inputName next → do
      H.modify (_ { inputName = inputName })
      pure next
    UpdateName inputName next → do
      s ← H.get
      handleFIDCallEff s.errorBus unit (F.setCurrentUserName s.inputName)
      H.modify (_ { inputName = "" })
      pure next
    CreateDebt fd next → do
      c ← H.gets _.creating
      H.modify (_ { creating = M.insert (F.getFriendAddr fd) fd c })
      pure next
    SendDebt creditor next → do
      s ← H.get
      let key = creditor
      case M.lookup key s.creating of
        Nothing   → pure next
        Just debt → do handleFIDCallEff s.errorBus unit (F.newPending debt)
                       H.modify (_ { creating = M.delete key s.creating })
                       pure next
    ConfirmPending debt next → do
      s ← H.get
      handleFIDCallEff s.errorBus unit (F.confirmPending debt)
      H.modify (_ { pending =
                      (filter (\fd → fd /= debt) s.pending)
                      <> [ F.setDebt debt (toNumber 0) ] })
      pure next
    CancelPending (F.FriendDebt debt) next → do
      s ← H.get
      handleFIDCallEff s.errorBus unit (F.cancelPending debt.friend)
      pure next
    RefreshDebts next → do
      errorBus ← H.gets _.errorBus
      loadFriendsAndDebts errorBus
      pure next

refreshButton =
  HH.button [ HE.onClick $ HE.input_ $ RefreshDebts
            , HP.class_ $ HH.ClassName "btn-info"]
  [ HH.text "Refresh" ]

loadFriendsAndDebts errorBus = do
  H.modify (_ { loading = true })
  friends     ← handleFIDCallAff errorBus [] F.currentUserFriends
  userName    ← handleFIDCallAff errorBus (Right "") F.getCurrentUserName
  names       ← handleFIDCallAff errorBus M.empty (F.allNames friends)
  debts       ← handleFIDCallAff errorBus [] (F.currentUserDebts friends)
  pending     ← handleFIDCallAff errorBus [] (F.currentUserPending friends)
  sentPending ← handleFIDCallAff errorBus [] (F.currentUserSentPendings friends)
  H.modify (_ { friends = friends, debts = debts, pending = pending, loading = false
              , sentPending = sentPending, names = names, userName = userName  })

displayFriendLi ∷ String → H.ComponentHTML Query
displayFriendLi n =
  HH.li [HP.class_ $ HH.ClassName "friend-row"]
  [HH.text n]

displayFriendDebtLi ∷ NameMap → F.FriendDebt → H.ComponentHTML Query
displayFriendDebtLi nm fd =
  HH.li [HP.class_ $ HH.ClassName $ moneyClass fd] $
  displayDebt nm fd

displayAllDebtLi ∷ NameMap → Tuple F.FriendDebt F.FriendDebt → H.ComponentHTML Query
displayAllDebtLi nm (Tuple fd1 fd2) =
  HH.li [HP.class_ $ HH.ClassName $ moneyClass fd1] $
  displayAllDebt nm (Tuple fd1 fd2)

displayFriendDebtSpan ∷ NameMap → Tuple F.FriendDebt F.FriendDebt → H.ComponentHTML Query
displayFriendDebtSpan nm  (Tuple originalDebt pendingDebt) =
  HH.div [HP.class_ $ HH.ClassName $ (moneyClass pendingDebt) <> " confirmation-body row"] $
  append (displayDebt nm pendingDebt) (displayDebtChanges nm (Tuple originalDebt pendingDebt))

displayAllDebt ∷ NameMap → Tuple F.FriendDebt F.FriendDebt → Array (H.ComponentHTML Query)
displayAllDebt nm (Tuple fd1 fd2) =
  let (Tuple (F.FriendDebt fdRec1) (F.FriendDebt fdRec2)) = Tuple fd1 fd2
      fName friend = fromMaybe (S.take 10 $ F.getUa fdRec1.friend) (M.lookup friend nm)
      combinedDebt = F.addDebt fd1 $ F.getDebt fd2
      compRes = F.posNegZero combinedDebt
      nameSpan n = HH.span [HP.class_ $ HH.ClassName "user-name user-id col-sm-8"] [HH.text n]
  in case compRes of
    F.Positive → [HH.div [HP.class_ $ HH.ClassName "confirmation-amount"][nameSpan $ (fName fdRec1.friend),
      HH.div [HP.class_ $ HH.ClassName "current-debt-amount"] [moneySpan combinedDebt, moneySpan fd2]]]
    F.Negative → [HH.div [HP.class_ $ HH.ClassName "confirmation-amount"][nameSpan $ fName fdRec1.friend,
      HH.div [HP.class_ $ HH.ClassName "current-debt-amount"] [moneySpan combinedDebt, moneySpan fd2]]]
    F.Zero     → [HH.div [HP.class_ $ HH.ClassName "confirmation-amount"][nameSpan $ fName fdRec1.friend ]]

displayDebt ∷ NameMap → F.FriendDebt → Array (H.ComponentHTML Query)
displayDebt nm (F.FriendDebt fd) =
  let fName friend = fromMaybe (S.take 10 $ F.getUa fd.friend) (M.lookup friend nm)
      compRes = F.posNegZero (F.FriendDebt fd)
      nameSpan n = HH.span [HP.class_ $ HH.ClassName "user-name user-id"] [HH.text n]
      fd' = F.FriendDebt fd
  in case compRes of
    F.Positive → [HH.div [HP.class_ $ HH.ClassName "confirmation-amount"][nameSpan $ (fName fd.friend) <> " ", moneySpan fd']]
    F.Negative → [HH.div [HP.class_ $ HH.ClassName "confirmation-amount"][nameSpan $ fName fd.friend <> " ", moneySpan fd' ]]
    F.Zero     → [HH.div [HP.class_ $ HH.ClassName "confirmation-amount"][nameSpan $ fName fd.friend <> " ", moneySpan fd' ]]

displayDebtChanges ∷ NameMap → Tuple F.FriendDebt F.FriendDebt → Array (H.ComponentHTML Query)
displayDebtChanges nm (Tuple originalDebt pendingDebt) =
   [HH.div
   [HP.class_ $ HH.ClassName "debt-changes"]
   [
     HH.span [HP.class_ $ HH.ClassName "originalDebt"] [moneySpan originalDebt],
     HH.span [HP.class_ $ HH.ClassName "changedDebt"] [moneySpan $ F.addDebt originalDebt $ F.getDebt pendingDebt]
   ]]

moneySpan ∷ F.FriendDebt → H.ComponentHTML Query
moneySpan (F.FriendDebt fd) =
  let debtCompare = F.posNegZero (F.FriendDebt fd)
      compRes = case debtCompare of
        F.Positive → "amount-payable"
        F.Negative → "amount-receivable"
        F.Zero     → "amount-nothing"
    in  HH.span [ HP.class_ $ HH.ClassName $ compRes ]
        [ HH.text $ show $ F.absMoney fd.debt ]

moneyClass ∷ F.FriendDebt → String
moneyClass fd = case F.posNegZero fd of
  F.Positive → "oweMoney"
  F.Negative → "owedMoney"
  F.Zero     → "zeroMoney"

reverseMoneyClass = moneyClass ∘ F.flipDebt

confirmButton ∷ F.FriendDebt → H.ComponentHTML Query
confirmButton fd = HH.button [ HP.class_ $ HH.ClassName "col-sm-6 btn-confirm"
                             , HE.onClick $ HE.input_ $ ConfirmPending fd]
  [ HH.text "Confirm" ]

cancelButton ∷ F.FriendDebt → H.ComponentHTML Query
cancelButton fd = HH.button [ HP.class_ $ HH.ClassName "col-sm-6 btn-cancel"
                             , HE.onClick $ HE.input_ $ CancelPending fd]
  [ HH.text "Cancel" ]

addFriendWidget ∷ State → H.ComponentHTML Query
addFriendWidget state =
  HH.div [ HP.class_ $ HH.ClassName "addFriend" ]
  [
    HH.input [ HP.type_ HP.InputText
             , HP.value $ inputVal state.newFriend
             , HE.onValueInput
               (HE.input (\val → InputFriend val))
             ]
  , HH.button [ HE.onClick $ HE.input_ $ AddFriend state.newFriend
              , HP.class_ $ HH.ClassName "btn-info"]
    [ HH.text "Add Friend by Address" ]
  ]
  where inputVal = either id F.getUa

nameChangeWidget ∷ String → Either F.UserAddress F.UserName → H.ComponentHTML Query
nameChangeWidget inputName userName =
  HH.div [ HP.class_ $ HH.ClassName "nameChange" ]
  [
    HH.input [ HP.type_ HP.InputText
             , HP.value inputName
             , HE.onValueInput (HE.input (\val → InputName val))
             ]
  , HH.button [ HE.onClick $ HE.input_ $ UpdateName inputName
              , HP.class_ $ HH.ClassName "btn-info"]
    [ HH.text $ "Change My Name from " <> (either F.getUa id userName) <>
      if (S.length inputName) > 0 then " to " <> inputName else "" ]
  ]

nonZero ∷ F.FriendDebt → Boolean
nonZero fd = (F.getDebt fd) /= (F.Money $ toNumber 0)

displayPending ∷ NameMap → Tuple F.FriendDebt F.FriendDebt → H.ComponentHTML Query
displayPending nm (Tuple originalDebt pendingDebt) = HH.li
  [ HP.class_ $ HH.ClassName "confirmation-row flipInX" ]
  [ HH.div [ HP.class_ $ HH.ClassName "confirmation-header row" ]
    [
      cancelButton pendingDebt,
      confirmButton pendingDebt
    ], displayFriendDebtSpan nm (Tuple originalDebt pendingDebt)
   ]

createDebt ∷ NameMap → DebtMap → F.UserAddress → H.ComponentHTML Query
createDebt nm creating friend =
  let fd = fromMaybe (F.friendDebtZero friend) $ M.lookup friend creating
  in HH.div [ HP.class_ $ HH.ClassName "createDebt" ]
     [ HH.text "$"
     , HH.input [ HP.type_ HP.InputNumber
                , HP.class_ $ HH.ClassName $ reverseMoneyClass $ fd
                , HP.value "0"
                , HE.onValueInput
                  (HE.input (\val → CreateDebt $ mkDebt friend val))
                , HP.min $ toNumber (-1000000)
                , HP.max $ toNumber 1000000]
     ,   HH.button [ HE.onClick $ HE.input_ $ SendDebt friend
                   , HP.class_ $ HH.ClassName "btn-info"]
         $ append [HH.text "Debt: "] (displayDebt nm $ F.flipDebt fd)
     ]

mkDebt ∷ F.UserAddress → String → F.FriendDebt
mkDebt friend debtStr = F.newDebt friend $ numberFromString debtStr

numberFromString ∷ String → Number
numberFromString s = fromMaybe (toNumber 0) (N.fromString s)

--helper to query the blockchain
--blankVal is a value to return if there's an error
--writes a message to the error bus if there's an error
handleFIDCallAff errorBus blankVal fidAffCall = do
  case errorBus of
    Nothing → do
      H.liftEff $ logShow "No bus initialized"
      pure blankVal
    Just b → do
      result ← H.liftAff fidAffCall
      case result of
        Left error → do _ ← H.liftAff $ Bus.write (FIDError error) b
                        pure blankVal
        Right val  → pure val

handleFIDCallEff errorBus blankVal fidAffCall = do
  case errorBus of
    Nothing → do
      H.liftEff $ logShow "No bus initialized"
      pure blankVal
    Just b → do
      result ← H.liftEff fidAffCall
      case result of
        Left error → do _ ← H.liftAff $ Bus.write (FIDError error) b
                        pure blankVal
        Right val  → pure val
