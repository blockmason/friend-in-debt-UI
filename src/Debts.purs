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
  | CreateDebt F.Debt a
  | SendDebt F.FoundationId a
  | ConfirmPending F.Debt a
  | CancelPending F.Debt a
  | AddFriend (Either String F.FoundationId) a
  | InputFriend String a
  | InputName String a
  | UpdateName String a
  | ShowItemizedDebtFor String a

type Input = ContainerMsgBus
type Message = String

type State = { friends     ∷ Array F.FoundationId
             , myId        ∷ F.FoundationId
             , debts       ∷ Array F.Debt
             , pending     ∷ Array F.Debt
             , sentPending ∷ Array F.Debt
             , creating    ∷ DebtMap
             , names       ∷ NameMap
             , newFriend   ∷ Either String F.FoundationId
             , userName    ∷ Either F.FoundationId F.UserName
             , inputName   ∷ String
             , showItemizedDebtFor :: String
             , defaultCurrency :: F.Currency
             , loading     ∷ Boolean
             , errorBus    ∷ ContainerMsgBus }

component ∷ ∀ eff. H.Component HH.HTML Query Input Message (FIDMonad eff)
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
                       , myId: (F.FoundationId "")
                       , pending: []
                       , sentPending: []
                       , creating: M.empty
                       , names:    M.empty
                       , newFriend: Left ""
                       , userName: (Right "")
                       , inputName: ""
                       , showItemizedDebtFor: ""
                       , defaultCurrency: F.USD
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
      $ append [
        -- HH.div
        -- [ HP.class_ $ HH.ClassName "refresh-button-container" ]
        -- [ refreshButton ],
      HH.div
        [ HP.class_ $ HH.ClassName "confirm-debts-container" ]
        [ HH.ul_ $ (displayPending state.names) <$> pending]
      , HH.h1_ [ HH.text "Current Debts"]
      -- , HH.div
      --   [ HP.class_ $ HH.ClassName "current-debts-container" ]
      --   [
      --     HH.ul_ $ (displayAllDebtLi state.names) <$>
      --     (zip state.debts state.sentPending)
      --   ]
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
          -- , HH.div
          --   [ HP.class_ $ HH.ClassName "name-change-container" ]
          --   [
          --     nameChangeWidget state.inputName state.userName
          --   ]
        ]
      , HH.div
        [ HP.class_ $ HH.ClassName "create-debt-container" ]
        [
          HH.h5_ [ HH.text "Create Debt" ]
        , HH.ul_ $ (\friend → HH.li_ [ createDebt state.names state.creating state.myId friend]) <$> state.friends
        ]
      ] $ (itemizedDebtsForFriendContainer state.showItemizedDebtFor) <$> friendNames
    where pending = filter (\(Tuple _ fd2) -> nonZero fd2) $ zip state.debts state.pending
          friendNames = fromFoldable $ M.values state.names

  eval ∷ Query ~> H.ComponentDSL State Query Message (FIDMonad eff)
  eval = case _ of
    ShowItemizedDebtFor name next → do
      H.modify (_ {showItemizedDebtFor = name})
      H.raise "show-itemized-debt"
      pure next
    HandleInput input next → do
      H.modify (_ { errorBus = input })
      pure next

    AddFriend eitherFriendId next → do
      s ← H.get
      case eitherFriendId of
        Left  str → pure next
        Right friendId → do
          H.modify (_ { newFriend = Left "" })
          handleFIDCall s.errorBus unit (F.createFriendship friendId)
          pure next
    InputFriend friendStr next → do
      if ((S.length friendStr) > 3) --id should be longer than 3 characters
        then H.modify (_ { newFriend = Right $ F.FoundationId friendStr })
        else H.modify (_ { newFriend = Left friendStr })
      pure next
    InputName inputName next → do
      H.modify (_ { inputName = inputName })
      pure next
    UpdateName inputName next → do
      s ← H.get
      handleFIDCall s.errorBus unit (F.setCurrentUserName s.inputName)
      H.modify (_ { inputName = "" })
      pure next
    CreateDebt fd next → do
      c ← H.gets _.creating
--      H.modify (_ { creating = M.insert (F.getFriendAddr fd) fd c })
      pure next
    SendDebt creditor next → do
      s ← H.get
      pure next

      -- let key = creditor
      -- case M.lookup key s.creating of
      --   Nothing   → pure next
      --   Just debt → do handleFIDCall s.errorBus unit (F.newPendingDebt debt)
      --                  H.modify (_ { creating = M.delete key s.creating })
      --                 pure next

    ConfirmPending debt next → do
      s ← H.get
  --    handleFIDCall s.errorBus unit (F.confirmPending debt)
--      H.modify (_ { pending =
--                      (filter (\fd → fd /= debt) s.pending)
--                      <> [ F.setDebt debt (toNumber 0) ] })
      pure next
    CancelPending (F.Debt debt) next → do
      s ← H.get
--      handleFIDCall s.errorBus unit (F.cancelPending debt.friend)
      pure next

    RefreshDebts next → do
      errorBus ← H.gets _.errorBus
--      loadFriendsAndDebts errorBus
      pure next


refreshButton =
  HH.button [ HE.onClick $ HE.input_ $ RefreshDebts
            , HP.class_ $ HH.ClassName "btn-info"]
  [ HH.text "Refresh" ]

loadFriendsAndDebts errorBus = do
  H.modify (_ { loading = true })
--  friends     ← handleFIDCall errorBus [] F.currentUserFriends
  userName    ← handleFIDCall errorBus (Right "") F.getCurrentUserName
--  names       ← handleFIDCall errorBus M.empty (F.allNames friends)
  let debts = []
      friends = []
      pending = []
      sentPending = []
      names = M.empty
--  debts       ← handleFIDCall errorBus [] (F.currentUserDebts friends)
--  pending     ← handleFIDCall errorBus [] (F.currentUserPending friends)
--  sentPending ← handleFIDCall errorBus [] (F.currentUserSentPendings friends)
  H.modify (_ { friends = friends, debts = debts, pending = pending, loading = false
              , sentPending = sentPending, names = names, userName = userName  })

itemizedDebtsForFriendContainer :: String → String → H.ComponentHTML Query
itemizedDebtsForFriendContainer friendToShow nm =
  HH.div
    [ HP.class_ $ HH.ClassName $ "itemized-debts-for-friend", HP.attr (HH.AttrName "style") $ if (nm == friendToShow) then "display: initial" else "display: none" ]
    [ HH.ul_ $ itemizedDebtLi <$> [F.mockDebt, F.mockDebt]]

displayFriendLi ∷ String → H.ComponentHTML Query
displayFriendLi n =
  HH.li [HP.class_ $ HH.ClassName "friend-row"]
  [HH.a [HP.href "#", HE.onClick $ HE.input_ $ ShowItemizedDebtFor n] [HH.text n]]

displayFriendDebtLi ∷ NameMap → F.Debt → H.ComponentHTML Query
displayFriendDebtLi nm fd =
  HH.li [HP.class_ $ HH.ClassName $ moneyClass fd] $
  displayDebt nm fd

-- displayAllDebtLi ∷ NameMap → Tuple F.Debt F.Debt → H.ComponentHTML Query
-- displayAllDebtLi nm (Tuple fd1 fd2) =
--   HH.li [HP.class_ $ HH.ClassName $ moneyClass fd1] $
--   displayAllDebt nm (Tuple fd1 fd2)

displayFriendDebtSpan ∷ NameMap → Tuple F.Debt F.Debt → H.ComponentHTML Query
displayFriendDebtSpan nm  (Tuple originalDebt pendingDebt) =
  HH.div [HP.class_ $ HH.ClassName $ (moneyClass pendingDebt) <> " confirmation-body row"] $
  append (displayDebt nm pendingDebt) (displayDebtChanges nm (Tuple originalDebt pendingDebt))

displayDebt ∷ NameMap → F.Debt → Array (H.ComponentHTML Query)
displayDebt nm (F.Debt fd) =
  let nameSpan n = HH.span [HP.class_ $ HH.ClassName "user-name user-id col-sm-8"] [HH.text $ show n]
      fd' = F.Debt fd
  in [HH.div [HP.class_ $ HH.ClassName "confirmation-amount"][nameSpan fd.creditor, moneySpan fd']]

itemizedDebtLi ∷ F.Debt → H.ComponentHTML Query
itemizedDebtLi fd =
  HH.li [HP.class_ $ HH.ClassName $ moneyClass fd] $
  itemizedDebt fd

itemizedDebt :: F.Debt → Array (H.ComponentHTML Query)
itemizedDebt fd =
  [HH.div [HP.class_ $ HH.ClassName "confirmation-amount"][descSpan fd, moneySpan fd]]

displayDebtChanges ∷ NameMap → Tuple F.Debt F.Debt → Array (H.ComponentHTML Query)
displayDebtChanges nm (Tuple originalDebt pendingDebt) =
   [HH.div
   [HP.class_ $ HH.ClassName "debt-changes"]
   [
     HH.span [HP.class_ $ HH.ClassName "originalDebt"] [moneySpan originalDebt],
     HH.span [HP.class_ $ HH.ClassName "changedDebt"] [moneySpan originalDebt]
   ]]

descSpan ∷ F.Debt → H.ComponentHTML Query
descSpan (F.Debt fd) =
  HH.span [] [ HH.text $ fd.desc ]

currencySpan ∷ F.Debt → H.ComponentHTML Query
currencySpan (F.Debt fd) =
  let (F.Money d) = fd.debt
  in HH.span [] [ HH.text $ show d.currency ]

moneySpan ∷ F.Debt → H.ComponentHTML Query
moneySpan (F.Debt fd) =
  HH.span [] [ HH.text $ show $ fd.debt ]

moneyClass ∷ F.Debt → String
moneyClass fd = "debt-amount"

confirmButton ∷ F.Debt → H.ComponentHTML Query
confirmButton fd = HH.button [ HP.class_ $ HH.ClassName "col-sm-6 btn-confirm"
                             , HE.onClick $ HE.input_ $ ConfirmPending fd]
  [ HH.text "Confirm" ]

cancelButton ∷ F.Debt → H.ComponentHTML Query
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
  where inputVal = either id show

nameChangeWidget ∷ String → Either F.EthAddress F.UserName → H.ComponentHTML Query
nameChangeWidget inputName userName =
  HH.div [ HP.class_ $ HH.ClassName "nameChange" ]
  [
    HH.input [ HP.type_ HP.InputText
             , HP.value inputName
             , HE.onValueInput (HE.input (\val → InputName val))
             ]
  , HH.button [ HE.onClick $ HE.input_ $ UpdateName inputName
              , HP.class_ $ HH.ClassName "btn-info"]
    [ HH.text $ "Change My Name from " <> (either F.getAddr id userName) <>
      if (S.length inputName) > 0 then " to " <> inputName else "" ]
  ]

nonZero ∷ F.Debt → Boolean
nonZero fd = ((F.numAmount ∘ F.fdDebt) fd) /= (toNumber 0)

displayPending ∷ NameMap → Tuple F.Debt F.Debt → H.ComponentHTML Query
displayPending nm (Tuple originalDebt pendingDebt) = HH.li
  [ HP.class_ $ HH.ClassName "confirmation-row flipInX" ]
  [ HH.div [ HP.class_ $ HH.ClassName "confirmation-header row" ]
    [
      cancelButton pendingDebt,
      confirmButton pendingDebt
    ], displayFriendDebtSpan nm (Tuple originalDebt pendingDebt)
   ]

createDebt ∷ NameMap → DebtMap → F.FoundationId → F.FoundationId → H.ComponentHTML Query
createDebt nm creating myId friend =
  let fd = fromMaybe (F.zeroDebt F.USD myId friend friend) $ M.lookup friend creating
  in HH.div [ HP.class_ $ HH.ClassName "createDebt" ]
     [ HH.text "$"
     , HH.input [ HP.type_ HP.InputNumber
--                , HP.class_ $ HH.ClassName $ reverseMoneyClass $ fd
                , HP.value "0"
--                , HE.onValueInput
--                  (HE.input (\val → CreateDebt $ mkDebt friend val))
                , HP.min $ toNumber (-1000000)
                , HP.max $ toNumber 1000000]
     ,   HH.button [ HE.onClick $ HE.input_ $ SendDebt friend
                   , HP.class_ $ HH.ClassName "btn-info"]
         $ append [HH.text "Debt: "] (displayDebt nm $ fd)
     ]

numberFromString ∷ String → Number
numberFromString s = fromMaybe (toNumber 0) (N.fromString s)

--helper to query the blockchain
--blankVal is a value to return if there's an error
--writes a message to the error bus if there's an error
handleFIDCall errorBus blankVal fidAffCall = do
  case errorBus of
    Nothing → do
      H.liftEff $ logShow "No bus initialized"
      pure blankVal
    Just b → do
      result ← H.liftAff $ F.runMonadF fidAffCall
      case result of
        Left error → do _ ← H.liftAff $ Bus.write (FIDError error) b
                        pure blankVal
        Right val  → pure val
