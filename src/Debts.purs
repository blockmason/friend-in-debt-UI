module Debts where


import FriendInDebt.Prelude
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
  | InputDebtAmount String a
  | InputDebtTarget String a
  | InputDebtDesc String a
  | AddDebt (Maybe F.Debt) a
  | ConfirmPending F.Debt a
  | CancelPending F.Debt a
  | AddFriend (Either String F.FoundationId) a
  | InputFriend String a
  | InputName String a
  | UpdateName String a
  | ShowItemizedDebtFor String a

type Input = ContainerMsgBus
type Message = String

type State = { friends             ∷ Array F.FoundationId
             , pendingFriendsTodo  ∷ Array F.FoundationId
             , pendingFriendsSent  ∷ Array F.FoundationId
             , balances            ∷ Array F.Balance
             , myId                ∷ F.FoundationId
             , pendingSent         ∷ Array F.Debt
             , pendingTodo         ∷ Array F.Debt
             , newDebt             ∷ Maybe F.Debt
             , names               ∷ NameMap
             , newFriend           ∷ Either String F.FoundationId
             , userName            ∷ Either F.FoundationId F.UserName
             , inputName           ∷ String
             , showItemizedDebtFor ∷ String
             , defaultCurrency     ∷ F.Currency
             , loading             ∷ Boolean
             , errorBus            ∷ ContainerMsgBus }

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
                       , pendingFriendsTodo: []
                       , pendingFriendsSent: []
                       , balances: []
                       , myId: F.fiBlankId
                       , pendingSent: []
                       , pendingTodo: []
                       , names:    M.empty
                       , newFriend: Left ""
                       , newDebt: Nothing
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
      HH.div
      [ HP.class_ $ HH.ClassName "page-container col-12" ]
      $ append [
      HH.div
        [ HP.class_ $ HH.ClassName "all-friends-container" ]
        [
          HH.ul
          [ HP.class_ $ HH.ClassName "col" ]
          $ (displayFriendLi ∘ F.fiGetId) <$> state.friends
        ]
      , HH.div
        [ HP.class_ $ HH.ClassName "all-balances-container" ]
        [
          HH.ul
          [ HP.class_ $ HH.ClassName "col-12" ]
          $ (displayBalanceLi mockMe) <$> [mockBalance, mockBalance]
        ]
      , HH.div
        [ HP.class_ $ HH.ClassName "all-pending-debts-container" ]
        [
          displaySentFriendsList state.pendingFriendsSent
        , displayTodoFriendsList state.pendingFriendsTodo
        , (displaySentDebtsList mockMe) [fakeDebt, fakeDebt]
        , (displayTodoList mockMe) [fakeDebt, fakeDebt]
        ]
      , HH.div
        [ HP.class_ $ HH.ClassName "all-settings-container" ]
        [
          HH.div
            [HP.class_ $ HH.ClassName "col default-currency-container"]
            [
              (HH.text  $ "Default Currency: "),
              HH.span [] [ HH.text $ show state.defaultCurrency ]
            ]
          , HH.div
            [HP.class_ $ HH.ClassName "col foundation-id-container"]
            [
              (HH.text  $ "My Foundation ID: "),
              HH.span [] [ HH.text $ show state.myId ]
            ]
        ]
      , HH.div
        [ HP.class_ $ HH.ClassName "add-friend-name-change-container" ]
        [
          HH.div
            [ HP.class_ $ HH.ClassName "add-friend-container" ]
            [
              addFriendWidget state
            ]
        ]
      , HH.div
        [ HP.class_ $ HH.ClassName "create-debt-container" ]
        [
          HH.h5_ [ HH.text "Create Debt" ]
        , HH.ul_ $ (\friend → HH.li [ HP.class_ $ HH.ClassName "row create-debt-card" ] [ inputDebt state.names state.myId state.newDebt ]) <$> [fakeFriend, fakeFriend]
        ]
      ] $ (itemizedDebtsForFriendContainer state.showItemizedDebtFor) <$> mockFriendNames
          where friendNames = fromFoldable $ M.values state.names

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
      hLog eitherFriendId
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
    InputDebtAmount strAmount next → do
      nd ← H.gets _.newDebt
      pure next
    InputDebtTarget strTarget next → do
      nd ← H.gets _.newDebt
      pure next
    InputDebtDesc strDesc next → do
      nd ← H.gets _.newDebt
      pure next
    AddDebt maybeDebt next → do
      s ← H.get
      pure next
    ConfirmPending debt next → do
      s ← H.get
      pure next
    CancelPending (F.Debt debt) next → do
      s ← H.get
--      handleFIDCall s.errorBus unit (F.cancelPending debt.friend)
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
  myId           ← handleFIDCall errorBus (F.FoundationId "") F.foundationId
  friends        ← handleFIDCall errorBus [] F.confirmedFriends
  pendingFriends ← handleFIDCall errorBus F.blankPendingFriends F.pendingFriends
  pendingD       ← handleFIDCall errorBus F.blankPendingDebts F.pendingDebts
  let names = M.empty
--  debts       ← handleFIDCall errorBus [] (F.currentUserDebts friends)
--  pending     ← handleFIDCall errorBus [] (F.currentUserPending friends)
  H.modify (_ { myId = myId, friends = friends
              , pendingFriendsSent = F.pfGetSents pendingFriends
              , pendingFriendsTodo = F.pfGetTodos pendingFriends
              , pendingSent = F.pdGetSents pendingD
              , pendingTodo = F.pdGetTodos pendingD
              , loading = false, names = names

              })

-- Itemized Debts for Friend Page
itemizedDebtsForFriendContainer :: String → String → H.ComponentHTML Query
itemizedDebtsForFriendContainer friendToShow nm =
  HH.div
    [ HP.class_ $ HH.ClassName $ "itemized-debts-for-friend", HP.attr (HH.AttrName "style") $ if (nm == friendToShow) then "display: block" else "display: none" ]
    [ HH.h5_ [ HH.text $ "History with " <> friendToShow <> ":" ],
      HH.ul_ $ itemizedDebtLi <$> [fakeDebt, fakeDebt]]

itemizedDebtLi ∷ F.Debt → H.ComponentHTML Query
itemizedDebtLi fd =
  HH.li [HP.class_ $ HH.ClassName $ moneyClass fd] $
  itemizedDebt fd

itemizedDebt :: F.Debt → Array (H.ComponentHTML Query)
itemizedDebt fd =
  [HH.div [HP.class_ $ HH.ClassName "itemized-debt-amount"][descSpan fd, debtAmountSpan fd]]

-- Friends List

displayFriendLi ∷ String → H.ComponentHTML Query
displayFriendLi n =
  HH.li [HP.class_ $ HH.ClassName "friend-row row"]
  [HH.a [HP.href "#", HE.onClick $ HE.input_ $ ShowItemizedDebtFor n] [HH.text n]]

-- Balance List

displayBalanceLi :: F.FoundationId → F.Balance → H.ComponentHTML Query
displayBalanceLi (me)(F.Balance bal) =
  HH.li [HP.class_ $ HH.ClassName "balance-row row align-items-center"]
  [
    HH.div [HP.class_ $ HH.ClassName "col creditor"][idSpan me bal.creditor],
    HH.div [HP.class_ $ HH.ClassName "col debtor"][idSpan me bal.debtor],
    HH.div [HP.class_ $ HH.ClassName "col amount"][verboseMoneySpan bal.amount]
  ]

-- Pending Friendships
displaySentFriendsList :: Array F.FoundationId → H.ComponentHTML Query
displaySentFriendsList friends = HH.ul_ $ displaySentFriendLi <$> friends

displaySentFriendLi ∷ F.FoundationId → H.ComponentHTML Query
displaySentFriendLi friend =
  HH.li [HP.class_ $ HH.ClassName "sent-friend-row row align-items-center"] $
    append
      (cardHeader "Waiting for response:")
      (displaySentFriend friend)

displaySentFriend :: F.FoundationId → Array (H.ComponentHTML Query)
displaySentFriend friend =
    [
      HH.div [HP.class_ $ HH.ClassName "row friend-details align-items-center"]
      [
        HH.div [HP.class_ $ HH.ClassName "col"][HH.text $ "Sent Friend Request to:" <> show friend]
      ]
    ]

displayTodoFriendsList :: Array F.FoundationId → H.ComponentHTML Query
displayTodoFriendsList friends = HH.ul_ $ displayTodoFriendLi <$> friends

displayTodoFriendLi ∷  F.FoundationId → H.ComponentHTML Query
displayTodoFriendLi friend =
  HH.li [HP.class_ $ HH.ClassName "todo-friend-row row align-items-center"]
  $ append
    (append
      (cardHeader "Respond to Request:")
      (displayTodoFriend friend))
  [
    HH.div [HP.class_ $ HH.ClassName "row action-buttons row align-items-center"][
      HH.div [HP.class_ $ HH.ClassName "col"][confirmFriendshipButton friend]
    ]
  ]

displayTodoFriend ::  F.FoundationId → Array (H.ComponentHTML Query)
displayTodoFriend friend =
    [
      HH.div [HP.class_ $ HH.ClassName "row friend-details row align-items-center"]
      [
        HH.div [HP.class_ $ HH.ClassName "col"][HH.text $ "Friend Request From:" <> show friend]
      ]
    ]

-- Pending Debts List
displaySentDebtsList :: F.FoundationId → Array F.Debt → H.ComponentHTML Query
displaySentDebtsList me debts = HH.ul_ $ (displaySentDebtLi me) <$> debts

displaySentDebtLi ∷ F.FoundationId → F.Debt → H.ComponentHTML Query
displaySentDebtLi me fd =
  HH.li [HP.class_ $ HH.ClassName "sent-debt-row row align-items-center"] $
    append
      (cardHeader "Waiting for Confirmation:")
      ((displaySentDebt me) fd)

displaySentDebt :: F.FoundationId → F.Debt → Array (H.ComponentHTML Query)
displaySentDebt me fd =
  let (F.Debt fd') = fd
  in
    [
      HH.div [HP.class_ $ HH.ClassName "row debt-details align-items-center"]
      [
        HH.div [HP.class_ $ HH.ClassName "col creditor"][idSpan me fd'.creditor],
        HH.div [HP.class_ $ HH.ClassName "col debtor"][idSpan me fd'.debtor],
        HH.div [HP.class_ $ HH.ClassName "col desc"][descSpan fd],
        HH.div [HP.class_ $ HH.ClassName "col amount"][debtAmountSpan fd]
      ]
    ]

displayTodoList :: F.FoundationId → Array F.Debt → H.ComponentHTML Query
displayTodoList me debts = HH.ul_ $ (displayTodoLi me) <$> debts

displayTodoLi ∷  F.FoundationId → F.Debt → H.ComponentHTML Query
displayTodoLi me fd =
  HH.li [HP.class_ $ HH.ClassName "todo-debt-row row align-items-center"]
  $ append
    (append
      (cardHeader "To Confirm:")
      ((displayTodo me) fd))
  [
    HH.div [HP.class_ $ HH.ClassName "row action-buttons row align-items-center"][
      HH.div [HP.class_ $ HH.ClassName "col"][cancelButton fd],
      HH.div [HP.class_ $ HH.ClassName "col"][confirmButton fd]
    ]
  ]

displayTodo ::  F.FoundationId → F.Debt → Array (H.ComponentHTML Query)
displayTodo me fd =
  let (F.Debt fd') = fd
  in
    [
      HH.div [HP.class_ $ HH.ClassName "row debt-details row align-items-center"]
      [
        HH.div [HP.class_ $ HH.ClassName "col-2 creditor"][idSpan me fd'.creditor],
        HH.div [HP.class_ $ HH.ClassName "col-2 debtor"][idSpan me fd'.debtor],
        HH.div [HP.class_ $ HH.ClassName "col-6 desc"][descSpan fd],
        HH.div [HP.class_ $ HH.ClassName "col-2 amount"][debtAmountSpan fd]
      ]
    ]

-- Helper components
cardHeader :: String -> Array (H.ComponentHTML Query)
cardHeader title =
  [
    HH.h3 [HP.class_ $ HH.ClassName "row card-title"][HH.text title]
  ]

idSpan :: F.FoundationId → F.FoundationId → H.ComponentHTML Query
idSpan me idToDisplay =
  let isItMe = me == idToDisplay
  in case isItMe of
    true → HH.text $ "Me"
    false → HH.a [HP.href "#", HE.onClick $ HE.input_ $ ShowItemizedDebtFor $ show idToDisplay] [HH.text $ show idToDisplay]

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

debtAmountSpan ∷ F.Debt → H.ComponentHTML Query
debtAmountSpan (F.Debt fd) =
  verboseMoneySpan fd.debt

verboseMoneySpan :: F.Money → H.ComponentHTML Query
verboseMoneySpan (F.Money d) =
  HH.span [] [ HH.text $ show d.currency <> " " <> show d.amount]

moneyClass ∷ F.Debt → String
moneyClass fd = "debt-amount"

confirmButton ∷ F.Debt → H.ComponentHTML Query
confirmButton fd = HH.button [ HP.class_ $ HH.ClassName "btn-confirm"
                             , HE.onClick $ HE.input_ $ ConfirmPending fd]
  [ HH.text "Confirm" ]

cancelButton ∷ F.Debt → H.ComponentHTML Query
cancelButton fd = HH.button [ HP.class_ $ HH.ClassName "btn-cancel"
                             , HE.onClick $ HE.input_ $ CancelPending fd]
  [ HH.text "Cancel" ]

confirmFriendshipButton :: F.FoundationId -> H.ComponentHTML Query
confirmFriendshipButton friend =
  HH.button [ HE.onClick $ HE.input_ $ AddFriend $ Right friend
            , HP.class_ $ HH.ClassName "confirm-friend-button"]
            [ HH.text "Confirm Friendship" ]

addFriendWidget ∷ State → H.ComponentHTML Query
addFriendWidget state =
  HH.div [ HP.class_ $ HH.ClassName "addFriend row col" ]
  [
    HH.input [ HP.type_ HP.InputText
             , HP.value $ inputVal state.newFriend
             , HP.class_ $ HH.ClassName "col"
             , HE.onValueInput
               (HE.input (\val → InputFriend val))
             ]
  , HH.button [ HE.onClick $ HE.input_ $ AddFriend state.newFriend
              , HP.class_ $ HH.ClassName "col-2"]
    [ HH.text "Add Friend by FoundationId" ]
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

inputDebt ∷ NameMap → F.FoundationId → Maybe F.Debt → H.ComponentHTML Query
inputDebt nm myId maybeDebt =
  HH.div [ HP.class_ $ HH.ClassName "createDebt col row" ]
  [
    HH.input [ HP.type_ HP.InputNumber
             , HP.class_ $ HH.ClassName "debt-amount col-2"
             , HP.value "0"
             , HE.onValueInput
               (HE.input (\val → InputDebtAmount val))
             , HP.min $ toNumber (-1000000)
             , HP.max $ toNumber 1000000]
  , HH.input [ HP.type_ HP.InputText
             , HP.class_ $ HH.ClassName "debt-description col "
             , HP.placeholder $ "debt description"
             , HE.onValueInput
               (HE.input (\val → InputDebtTarget val))
             , HP.value ""]
  , HH.input [ HP.type_ HP.InputText
             , HP.placeholder $ "debt description"
             , HE.onValueInput
               (HE.input (\val → InputDebtDesc val))
             , HP.value ""]
  , HH.button [ HE.onClick $ HE.input_ $ AddDebt maybeDebt
              , HP.class_ $ HH.ClassName "create-debt-button col-2"]
    [HH.text $ "Debt " <> (show $ (F.debtCounterparty myId) <$> maybeDebt) ]
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

-- Mocks for Testing purposes
mockFriendNames :: Array String
mockFriendNames = ["bob", "tim", "kevin"]

mockFriends :: Array F.FoundationId
mockFriends = [F.FoundationId "bob", F.FoundationId "Tim", F.FoundationId "Kevin"]

mockNameMap :: NameMap
mockNameMap = M.insert (F.FoundationId "bob") "Bob Brown" $ M.empty

fakeDebt :: F.Debt
fakeDebt = mockDebt $ F.FoundationId "bob"

mockMe :: F.FoundationId
mockMe = (F.FoundationId "me")

fakeFriend :: F.FoundationId
fakeFriend = (F.FoundationId "bob")

mockDebtMap :: DebtMap
mockDebtMap = M.insert (F.FoundationId "bob") fakeDebt $ M.empty

mockBalance :: F.Balance
mockBalance = F.Balance { debtor: mockMe, creditor: fakeFriend, amount: F.Money {amount: 5.0, currency: F.USD}}

mockPendingDebts :: F.PendingDebts
mockPendingDebts = F.PD {sent: [fakeDebt], todo: [fakeDebt]}

mockFoundationId :: F.FoundationId
mockFoundationId = F.FoundationId "snoopy"
mockDebt :: F.FoundationId -> F.Debt
mockDebt fid = F.mkDebt mockFoundationId fid fid (F.mkMoney 2.0 F.USD) F.NoDebtId "Fictional Cat Poop"
