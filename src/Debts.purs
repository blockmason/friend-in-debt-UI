module Debts where


import FriendInDebt.Prelude
import FriendInDebt.Types (FIDMonad, ContainerMsgBus, ContainerMsg(..), NameMap, DebtsMap)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Aff (Aff)
import Data.Array (singleton, head)
import Control.Monad.Aff.Bus as Bus
import Data.Int    as I
import Data.Number as N
import Data.String as S
import Data.Map    as M
import Data.Array (length, filter, zip, fromFoldable, groupBy, sortBy, find)
import Data.NonEmpty
import DOM.HTML.Indexed.StepValue (StepValue(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import FriendInDebt.Blockchain          (handleCall, handleTx, hasNetworkError, formatDate, shortDate)
import Network.Eth.FriendInDebt         as F
import Network.Eth                      as E
import UI.IconGenerator as ICON
import UI.UIStatesKit as UIStates

import FriendInDebt.Routes              as R

data Query a
  = RefreshDebts a
  | HandleInput Input a
  | InputDebtDetails DebtType DebtInfo a
  | AddDebt DebtType F.Debt a
  | ConfirmPending F.Debt a
  | RejectPending F.Debt a
  | AddFriend String a
  | ConfirmFriend F.FoundationId a
  | CancelFriend F.FoundationId a
  | InputFriend String a
  | ShowItemizedDebtFor (Maybe F.FoundationId) a
  | NoOp a

type Input = ContainerMsgBus

newtype ErrorFlash = ErrorFlash { message ∷ String, intrusive ∷ String }

data Message
  = ScreenChange R.Screen
  | NewTX E.TX
  | NumPendingTodo Int
  | NumPendingFriends Int
  | LoadId F.FoundationId
newtype FriendBundle = FriendBundle { id ∷ F.FoundationId, gradient ∷ ICON.GradientCss, balance ∷ Maybe F.Balance }

type DebtInfo = { amount ∷ String, desc ∷ String, counterParty ∷ String }
blankDebtInfo = { amount: "", desc: "", counterParty: "" }
type State = { friends             ∷ Array F.FoundationId
             , gradients           ∷ Array ICON.GradientCss
             , pendingFriendsTodo  ∷ Array F.FoundationId
             , pendingFriendsSent  ∷ Array F.FoundationId
             , balances            ∷ Array F.Balance
             , itemizedDebts       ∷ DebtsMap
             , myId                ∷ F.FoundationId
             , pendingSent         ∷ Array F.Debt
             , pendingTodo         ∷ Array F.Debt
             , newDebtInput        ∷ DebtInfo
             , newCreditInput      ∷ DebtInfo
             , newFriend           ∷ String
             , userName            ∷ Either F.FoundationId F.UserName
             , inputName           ∷ String
             , nameNoExist         ∷ Boolean
             , showItemizedDebtFor ∷ Maybe F.FoundationId
             , defaultCurrency     ∷ F.Currency
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
                       , gradients: []
                       , pendingFriendsTodo: []
                       , pendingFriendsSent: []
                       , balances: []
                       , itemizedDebts: M.empty
                       , myId: F.fiBlankId
                       , pendingSent: []
                       , pendingTodo: []
                       , newFriend: ""
                       , newDebtInput: blankDebtInfo
                       , newCreditInput: blankDebtInfo
                       , userName: (Right "")
                       , inputName: ""
                       , nameNoExist: false
                       , showItemizedDebtFor: Nothing
                       , defaultCurrency: F.cUSD
                       , errorBus: input }

  render ∷ State → H.ComponentHTML Query
  render state =
    HH.div
      [ HP.class_ $ HH.ClassName "page-container col-12" ]
      [
        page R.FriendsScreen $
             HH.ul [ HP.class_ $ HH.ClassName "col" ]
             $ groupFriendLiByInitial state.myId state.defaultCurrency $ prepareFriendBundles state

      , page R.BalancesScreen $
             HH.ul
             [ HP.class_ $ HH.ClassName "col-12" ]
             $ case (length state.balances) of
                0 →
                  [ emptyBalance ]
                _ →
                  (displayBalanceLi state) <$> state.balances
      , page R.PendingScreen $ pendingPage state
      , page R.SettingsScreen $ settingsPage state
      , page R.AddFriendScreen $
              HH.div
                [ HP.class_ $ HH.ClassName "add-friend-container" ]
                [
                  addFriendWidget state
                ]
      , page R.CreateDebtScreen $ createDebtModal state
      ]

  eval ∷ Query ~> H.ComponentDSL State Query Message (FIDMonad eff)
  eval = case _ of
    NoOp next → pure next
    ShowItemizedDebtFor maybeFriend next → do
      H.raise $ ScreenChange R.ItemizedDebtsScreen

      case maybeFriend of
        Nothing → pure next
        Just f  → do
          s ← H.get
          H.modify (_ { showItemizedDebtFor = maybeFriend })
          H.liftEff $ UIStates.toggleLoading(".itemized-debts")

          idebts ← handleCall s.errorBus [] (F.itemizedDebts f)
          H.modify (_ { itemizedDebts = M.insert f idebts s.itemizedDebts })
          H.liftEff $ UIStates.toggleLoading(".itemized-debts")

          pure next

    HandleInput input next → do
      state <- H.get
      case (length state.gradients) of
        0 → do
          gradients <- H.liftEff $ sequence $ (const ICON.randomGradient) <$> state.friends
          H.modify (_ { errorBus = input, gradients = gradients })
          pure next
        _ → do
          H.modify (_ { errorBus = input })
          pure next
    ConfirmFriend friend next → do
      s ← H.get

      H.liftEff $ UIStates.toggleLoading(".confirm-friend-button")
      handleTx NewTX s (ScreenChange R.BalancesScreen) $ F.createFriendship friend
      H.liftEff $ UIStates.toggleLoading(".confirm-friend-button")

      pure next
    CancelFriend friend next → do
      s ← H.get
      H.liftEff $ UIStates.toggleLoading(".cancel-friend-button")
      handleTx NewTX s (ScreenChange R.BalancesScreen) $ F.deleteFriendship friend
      H.liftEff $ UIStates.toggleLoading(".cancel-friend-button")
      pure next
    AddFriend friendStr next → do
      s ← H.get
      nameInUse ← handleCall s.errorBus false $ F.nameInUse friendStr
      if not nameInUse
        then do

          H.modify (_ { nameNoExist = true, newFriend = "" })
          hLog $ friendStr <> " does not exist."
          H.liftEff $ UIStates.toggleError(".add-friend-input")
        else do
          H.liftEff $ UIStates.toggleLoading(".add-friend-button")
          handleTx NewTX s (ScreenChange R.BalancesScreen) $
            F.createFriendship $ F.fiMkId friendStr
          H.modify (_ { newFriend = "" })
          H.liftEff $ UIStates.toggleLoading(".add-friend-button")
      pure next
    InputFriend friendStr next → do
      H.modify (_ { nameNoExist = false })
      if F.fiStrValidId (S.toLower friendStr) || S.length friendStr < 4
        then H.modify (_ { newFriend = S.toLower friendStr })
        else H.modify (_ { newFriend = "" })
      pure next
    InputDebtDetails debtType debtInfo next → do
      case debtType of
        Debt   → H.modify (_ { newDebtInput   = debtInfo })
        Credit → H.modify (_ { newCreditInput = debtInfo})
      pure next
    AddDebt debtType debt next → do
      hLog debt
      H.liftEff $ UIStates.toggleLoading(".create-debt-button")
      s ← H.get
      handleTx NewTX s (ScreenChange R.BalancesScreen) $ F.newPendingDebt debt
      H.modify (_ { newDebtInput   = blankDebtInfo
                  , newCreditInput = blankDebtInfo })
      H.liftEff $ UIStates.toggleLoading(".create-debt-button")
      pure next
    ConfirmPending debt next → do
      s ← H.get
      H.liftEff $ UIStates.toggleLoading(".confirm-pending-button")
      handleTx NewTX s (ScreenChange R.BalancesScreen) $ F.confirmPendingDebt debt
      H.liftEff $ UIStates.toggleLoading(".confirm-pending-button")
      pure next
    RejectPending debt next → do
      s ← H.get
      H.liftEff $ UIStates.toggleLoading(".reject-pending-button")
      handleTx NewTX s (ScreenChange R.BalancesScreen) $ F.rejectPendingDebt debt
      H.liftEff $ UIStates.toggleLoading(".reject-pending-button")
      pure next
    RefreshDebts next → do
      H.liftEff $ UIStates.toggleLoading(".page-container")
      errorBus    ← H.gets _.errorBus
      loadFriendsAndDebts errorBus
      H.liftEff $ UIStates.toggleLoading(".page-container")
      s ← H.get
      H.raise $ NumPendingTodo    (length s.pendingTodo)
      H.raise $ NumPendingFriends (length s.pendingFriendsTodo)
      pure next

refreshButton =
  HH.button [ HE.onClick $ HE.input_ $ RefreshDebts
            , HP.class_ $ HH.ClassName "btn-info"]
  [ HH.text "Refresh" ]

loadFriendsAndDebts errorBus = do
  myId           ← handleCall errorBus (F.FoundationId "") F.foundationId
  friends        ← handleCall errorBus [] F.confirmedFriends
  pendingFriends ← handleCall errorBus F.blankPendingFriends F.pendingFriends
  pendingD       ← handleCall errorBus F.blankPendingDebts F.pendingDebts
  balances       ← handleCall errorBus [] F.debtBalances
  H.modify (_ { myId = myId, friends = friends
              , pendingFriendsSent = F.pfGetSents pendingFriends
              , pendingFriendsTodo = F.pfGetTodos pendingFriends
              , pendingSent = F.pdGetSents pendingD
              , pendingTodo = F.pdGetTodos pendingD
              , balances = balances
              })
  H.raise $ LoadId myId

-- structural components

page ∷ R.Screen → H.ComponentHTML Query → H.ComponentHTML Query
page screen child =
  HH.div
    [HP.class_ (HH.ClassName $ R.getContainerNameFor screen)]
    [child]

-- pages

createDebtModal ∷ State → H.ComponentHTML Query
createDebtModal state =
  HH.div
    [ HP.class_ $ HH.ClassName "create-debt-container" ]
    [
      HH.h6 [HP.class_ $ HH.ClassName "modal-title"] [ HH.text "Create Debt" ]
    , HH.ul
    [ HP.class_ $ HH.ClassName "col" ]
    [
        HH.li [ HP.class_ $ HH.ClassName "row create-debt-card" ]
          [inputDebt state.defaultCurrency state.myId state.friends state.newDebtInput]
      , HH.li [ HP.class_ $ HH.ClassName "row create-debt-card" ]
          [inputCredit state.defaultCurrency state.myId state.friends state.newCreditInput]
      ]
    ]

settingsPage ∷ State → H.ComponentHTML Query
settingsPage state =
    HH.ul
    [ HP.class_ $ HH.ClassName "col-12" ]
    [
      HH.div
        [HP.class_ $ HH.ClassName "row default-currency-container"]
        [
          (HH.text  $ "Default Currency:  "),
          HH.span [] [ HH.text $ show state.defaultCurrency ]
        ]
      , HH.div
        [HP.class_ $ HH.ClassName "row foundation-id-container"]
        [
          (HH.text  $ "My Foundation ID:  "),
          HH.span [] [ HH.text $ show state.myId ]
        ]
    ]

pendingPage ∷ State → H.ComponentHTML Query
pendingPage state =
     HH.div
    [ HP.class_ $ HH.ClassName "" ]
    [
      displaySentFriendsList state.pendingFriendsSent
    , displayTodoFriendsList state.pendingFriendsTodo
    , (displaySentDebtsList state.myId) state.pendingSent
    , (displayTodoList state.myId) state.pendingTodo
    ]


-- Itemized Debts for Friend Page
displayItemizedDebtTimeline :: Maybe F.FoundationId → DebtsMap → F.FoundationId → H.ComponentHTML Query
displayItemizedDebtTimeline friendToShow debtsMap me =
  case friendToShow of
    Just f →
      HH.div
        [ HP.class_ $ HH.ClassName $ "col itemized-debts"]
        [
          HH.div [HP.class_ $ HH.ClassName "row"][HH.text "Debt History:"],
          HH.div [HP.class_ $ HH.ClassName "row"][HH.h6_ [HH.text $ show f]],
          HH.ul [HP.class_ $ HH.ClassName "row debt-timeline"]
            $ (itemizedDebtLi me) <$> (fromMaybe [] $ M.lookup f debtsMap)
        ]
    Nothing → HH.div_ [ HH.text "" ]

itemizedDebtLi ∷ F.FoundationId → F.Debt → H.ComponentHTML Query
itemizedDebtLi me fd =
  let mostRecent  = maybe "" shortDate $ F.debtTimestamp fd
      amIcreditor = (F.debtCreditor fd) == me

  in
    HH.li [HP.class_ $ HH.ClassName $ "timeline-event " <> (if amIcreditor then "positive" else "negative"),
           HP.attr (HH.AttrName "data-timestamp") mostRecent
          ] $ itemizedDebt fd

itemizedDebt :: F.Debt → Array (H.ComponentHTML Query)
itemizedDebt fd =
  [
    HH.div [HP.class_ $ HH.ClassName "event-description"][descSpan fd],
    HH.div [HP.class_ $ HH.ClassName "event-amount"][debtAmountSpan fd]
  ]

-- Friends List

findBalanceFor ∷ F.FoundationId → Array F.Balance → Maybe F.Balance
findBalanceFor fid balances =
  find (\(F.Balance balance) → balance.debtor == fid || balance.creditor == fid) balances

prepareFriendBundles ∷ State → Array FriendBundle
prepareFriendBundles state =
  (\(Tuple fid1 gradient) → FriendBundle {id: fid1, gradient: gradient, balance: findBalanceFor fid1 state.balances}) <$> (zip state.friends state.gradients)

groupFriendLiByInitial ∷ F.FoundationId → F.Currency → Array FriendBundle → Array (H.ComponentHTML Query)
groupFriendLiByInitial me c friendBundles =
  let
    orderedFriends = sortBy (\(FriendBundle bundle1) (FriendBundle bundle2) → S.localeCompare (F.initial bundle1.id) (F.initial bundle2.id)) friendBundles
    friendGroups = groupBy (\(FriendBundle bundle1) (FriendBundle bundle2) → (F.initial bundle1.id) == (F.initial bundle2.id)) orderedFriends
  in
    (displayFriendGroup me c) <$> friendGroups

displayFriendGroup ∷ F.FoundationId → F.Currency → NonEmpty Array FriendBundle → H.ComponentHTML Query
displayFriendGroup me c group =
 let
  innerArr = oneOf group
  initial = fromMaybe "" $ do
      (FriendBundle bundle1) ← head innerArr
      pure $ F.initial bundle1.id

 in HH.div [HP.class_ $ HH.ClassName "row initial-group"]
           [
             HH.div [HP.class_ $ HH.ClassName "col-1"]
                    [HH.h6 [HP.class_ $ HH.ClassName "initial-label"] [HH.text initial]]
            , HH.div [HP.class_ $ HH.ClassName "col"]
                     $ (displayFriendLi me c) <$> innerArr
            ]

displayFriendLi ∷ F.FoundationId → F.Currency → FriendBundle → H.ComponentHTML Query
displayFriendLi me c (FriendBundle bundle) =
  let amount = maybe (F.mkMoney 0.0 c) F.balAmount bundle.balance
      debtor = fromMaybe F.fiBlankId $ F.balDebtor <$> bundle.balance
  in HH.li [HP.class_ $ HH.ClassName "friend-item row"]
    [
      HH.div [HP.class_ $ HH.ClassName "col-3"]
        [ICON.generatedIcon (show bundle.id) bundle.gradient]
      , HH.div [HP.class_ $ HH.ClassName "col-9 name-portion"]
        [
          HH.text $ show bundle.id,
          HH.span [HP.class_ $ HH.ClassName $
                   "balance-amount" <> if me == debtor then " debt" else " credit"]
          [HH.text $ "Balance: " <> F.formatMoney amount ]
        ]
    ]

-- Balance List

emptyBalance :: H.ComponentHTML Query
emptyBalance =
  HH.div [HP.class_ $ HH.ClassName "no-balances"][HH.text "No Balances Found, Check Pending for Unconfirmed Debts"]

displayBalanceLi :: State → F.Balance → H.ComponentHTML Query
displayBalanceLi state bal =
  let debtsMap = state.itemizedDebts
      me       = state.myId
      creditor = F.balCreditor bal
      debtor   = F.balDebtor bal
      amount   = F.balAmount bal
      totalDebts  = F.balTotalDebts bal
      debtStr  = if totalDebts == 1 then " debt" else " debts"
      mostRecent  = maybe "" formatDate $ F.balMostRecent bal
      curFriend = if creditor == me then debtor else creditor
      status    = if creditor == me then "Owed to Me" else "I Owe"
      friendToShow = state.showItemizedDebtFor
      expandClass = (\f → if f == curFriend then "expand-itemized" else "hide-itemized") <$> friendToShow
  in
    HH.li [HP.class_ $ HH.ClassName $ "balance-row row " <> fromMaybe "" expandClass,
           HE.onClick $ HE.input_ $ ShowItemizedDebtFor $ Just curFriend]
    $ [
      HH.div [HP.class_ $ HH.ClassName "col-4 debt-excerpt"][
        HH.div [HP.class_ $ HH.ClassName $
                "row debt-amount" <> if creditor == me then " credit" else " debt"
               ]
        [moneySpan amount],
        HH.div [HP.class_ $ HH.ClassName "row label-row"][HH.small_[HH.text "last"]],
        HH.div [HP.class_ $ HH.ClassName "row thin-item-row"]
          [
            HH.span [HP.class_ $ HH.ClassName "thin-item"]
            [HH.text mostRecent]
          ]
      ],
      HH.div [HP.class_ $ HH.ClassName "col debt-details"][
        HH.div [HP.class_ $ HH.ClassName "col debt-relationship"][
          HH.div [HP.class_ $ HH.ClassName "row"][HH.text $ show curFriend],
          HH.div [HP.class_ $ HH.ClassName "row"][HH.h6_ [HH.text $ ""]]
        ],
        HH.div [HP.class_ $ HH.ClassName "row label-row"][
          HH.div [HP.class_ $ HH.ClassName "col-6"][HH.small_[HH.text "currency"]],
          HH.div [HP.class_ $ HH.ClassName "col-6"][HH.small_[HH.text "debts"]]
        ],
        HH.div [HP.class_ $ HH.ClassName "row thin-item-row"][
          HH.div [HP.class_ $ HH.ClassName "col thin-item"][currencySpan amount],
          HH.div [HP.class_ $ HH.ClassName "col thin-item"][HH.text $ show totalDebts <> debtStr]
        ]
      ],
      (displayItemizedDebtTimeline friendToShow debtsMap me)
    ]

-- Pending Friendships
displaySentFriendsList :: Array F.FoundationId → H.ComponentHTML Query
displaySentFriendsList friends = HH.ul [HP.class_ $ HH.ClassName "col"] $ displaySentFriendLi <$> friends

displaySentFriendLi ∷ F.FoundationId → H.ComponentHTML Query
displaySentFriendLi friend =
  HH.li [HP.class_ $ HH.ClassName "sent-friend-row row"] $
    append
      (cardHeader "Waiting for response:")
      (displaySentFriend friend)

displaySentFriend :: F.FoundationId → Array (H.ComponentHTML Query)
displaySentFriend friend =
    [
      HH.div [HP.class_ $ HH.ClassName "row friend-details"]
      [
        HH.div [HP.class_ $ HH.ClassName "request-details"]
        [
          HH.span_ [HH.text "Friend Request To"],
          HH.span_ [HH.text $ show friend]
--          , HH.span_ [HH.text "2017/07/29 15:35"]
        ]
      ]
    ]

displayTodoFriendsList :: Array F.FoundationId → H.ComponentHTML Query
displayTodoFriendsList friends = HH.ul [HP.class_ $ HH.ClassName "col"] $ displayTodoFriendLi <$> friends

displayTodoFriendLi ∷  F.FoundationId → H.ComponentHTML Query
displayTodoFriendLi friend =
  HH.li [HP.class_ $ HH.ClassName "todo-friend-row row align-items-center"]
  $ append
    (append
      (cardHeader "Respond to Request:")
      (displayTodoFriend friend))
  [
    HH.div [HP.class_ $ HH.ClassName "row action-buttons row align-items-center"][
      HH.div [HP.class_ $ HH.ClassName ""][cancelFriendshipButton friend]
      , HH.div [HP.class_ $ HH.ClassName ""][confirmFriendshipButton friend]
      ]
  ]

displayTodoFriend ∷ F.FoundationId → Array (H.ComponentHTML Query)
displayTodoFriend friend =
    [
      HH.div [HP.class_ $ HH.ClassName "row friend-details"]
      [
        HH.div [HP.class_ $ HH.ClassName "request-details"]
        [
          HH.span_ [HH.text "Friend Request From"],
          HH.span_ [HH.text $ show friend]
        ]
      ]
    ]

-- Pending Debts List
displaySentDebtsList :: F.FoundationId → Array F.Debt → H.ComponentHTML Query
displaySentDebtsList me debts = HH.ul [HP.class_ $ HH.ClassName "col"] $ (displaySentDebtLi me) <$> debts

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
        HH.div [HP.class_ $ HH.ClassName "col-4 debt-relationship"]
        [
          HH.div [HP.class_ $ HH.ClassName "debtor"][idSpan me fd'.debtor],
          debtRelationshipSpan me fd,
          HH.div [HP.class_ $ HH.ClassName "creditor"][idSpan me fd'.creditor]
        ]
        , HH.div [HP.class_ $ HH.ClassName "col-8 debt-desc"]
        [
          HH.div [HP.class_ $ HH.ClassName "desc"][descSpan fd],
          HH.div [HP.class_ $ HH.ClassName "amount"][debtAmountSpan fd]
        ]
      ]
    ]

displayTodoList :: F.FoundationId → Array F.Debt → H.ComponentHTML Query
displayTodoList me debts = HH.ul [HP.class_ $ HH.ClassName "col"] $ (displayTodoLi me) <$> debts

displayTodoLi ∷  F.FoundationId → F.Debt → H.ComponentHTML Query
displayTodoLi me fd =
  HH.li [HP.class_ $ HH.ClassName "todo-debt-row row align-items-center"]
  $ append
    (append
      (cardHeader "To Confirm:")
      ((displayTodo me) fd))
  [
    HH.div [HP.class_ $ HH.ClassName "row action-buttons align-items-end"][
      HH.div [HP.class_ $ HH.ClassName ""][cancelButton fd],
      HH.div [HP.class_ $ HH.ClassName ""][confirmButton fd]
    ]
  ]

displayTodo ::  F.FoundationId → F.Debt → Array (H.ComponentHTML Query)
displayTodo me fd =
  let (F.Debt fd') = fd
  in
    [
      HH.div [HP.class_ $ HH.ClassName "row debt-details row align-items-center"]
      [
        HH.div [HP.class_ $ HH.ClassName "col-4 debt-relationship"]
        [
          HH.div [HP.class_ $ HH.ClassName "debtor"][idSpan me fd'.debtor],
          debtRelationshipSpan me fd,
          HH.div [HP.class_ $ HH.ClassName "creditor"][idSpan me fd'.creditor]
        ]
        , HH.div [HP.class_ $ HH.ClassName "col-8 debt-desc"]
        [
          HH.div [HP.class_ $ HH.ClassName "desc"][descSpan fd],
          HH.div [HP.class_ $ HH.ClassName "amount"][debtAmountSpan fd]
        ]
      ]
    ]

-- Helper components
cardHeader :: String -> Array (H.ComponentHTML Query)
cardHeader title =
  [
    HH.h3 [HP.class_ $ HH.ClassName "row card-title"][HH.text title]
  ]

debtRelationshipSpan ∷ F.FoundationId → F.Debt → H.ComponentHTML Query
debtRelationshipSpan me fd =
  let relationship = if (F.debtDebtor fd) == me then "owe" else "owes"
  in
    HH.span_ [HH.text relationship]


idSpan :: F.FoundationId → F.FoundationId → H.ComponentHTML Query
idSpan me idToDisplay =
  let isItMe = me == idToDisplay
  in case isItMe of
    true → HH.text $ "You"
    false → HH.a [HP.class_ $ HH.ClassName "expandable-id", HP.href "#", HE.onClick $ HE.input_ $ ShowItemizedDebtFor $ Just idToDisplay] [HH.text $ show idToDisplay]

descSpan ∷ F.Debt → H.ComponentHTML Query
descSpan (F.Debt fd) =
  HH.span [] [ HH.text $ fd.desc ]

currencySpan ∷ F.Money → H.ComponentHTML Query
currencySpan (F.Money m) =
  HH.span [HP.class_ $ HH.ClassName "currency-span"] [ HH.text $ show m.currency ]

moneySpan ∷ F.Money → H.ComponentHTML Query
moneySpan m =
  HH.span [HP.class_ $ HH.ClassName "money-span"] [ HH.text $ F.formatMoney m ]

debtAmountSpan ∷ F.Debt → H.ComponentHTML Query
debtAmountSpan (F.Debt fd) =
  verboseMoneySpan fd.debt

verboseMoneySpan :: F.Money → H.ComponentHTML Query
verboseMoneySpan m =
  HH.span [] [
    currencySpan m,
    moneySpan m
  ]

moneyClass ∷ F.Debt → String
moneyClass fd = "debt-amount"

confirmButton ∷ F.Debt → H.ComponentHTML Query
confirmButton fd = HH.button [ HP.class_ $ HH.ClassName "fa fa-check confirm-pending-button"
                             , HE.onClick $ HE.input_ $ ConfirmPending fd] []

cancelButton ∷ F.Debt → H.ComponentHTML Query
cancelButton fd = HH.button [ HP.class_ $ HH.ClassName "fa fa-close reject-pending-button"
                             , HE.onClick $ HE.input_ $ RejectPending fd] []

confirmFriendshipButton :: F.FoundationId -> H.ComponentHTML Query
confirmFriendshipButton friend =
  HH.button [ HE.onClick $ HE.input_ $ ConfirmFriend friend
            , HP.class_ $ HH.ClassName "confirm-friend-button"]
            [ HH.i [HP.class_ $ HH.ClassName "fa fa-check"][] ]

cancelFriendshipButton :: F.FoundationId -> H.ComponentHTML Query
cancelFriendshipButton friend =
  HH.button [ HE.onClick $ HE.input_ $ CancelFriend friend
            , HP.class_ $ HH.ClassName "cancel-friend-button"]
              [ HH.i [HP.class_ $ HH.ClassName "fa fa-close"][]]

addFriendWidget ∷ State → H.ComponentHTML Query
addFriendWidget state =
  HH.div [ HP.class_ $ HH.ClassName "add-friend" ]
  [
    HH.h6 [ HP.class_ $ HH.ClassName "modal-title"][HH.text "Add New Friend"],
    HH.label [][HH.text "Friend's FoundationID"],
    HH.input [ HP.type_ HP.InputText
             , HP.value $ state.newFriend
             , HP.class_ $ HH.ClassName "form-control add-friend-input"
             , HP.placeholder $ "johndoe"
             , HP.attr (HH.AttrName "maxlength") "32"
             , HE.onValueInput
               (HE.input (\val → InputFriend val))
             ]
  , HH.button [ HE.onClick $ HE.input_ $ AddFriend state.newFriend
              , HP.class_ $ HH.ClassName "form-control add-friend-button"
              , HP.enabled $ F.fiStrValidId state.newFriend ]
    [ HH.text "Add Friend by FoundationId" ]
  ]

nonZero ∷ F.Debt → Boolean
nonZero fd = ((F.numAmount ∘ F.debtMoney) fd) /= 0.0

instance showDebtType :: Show DebtType where
show debtType =
  case debtType of
    Debt → "Debt You Owe"
    Credit → "Debt Owed To You"

data DebtType = Debt | Credit
--Boolean passed is just to force a refresh
inputFDebt ∷ DebtType → F.Currency → F.FoundationId → Array F.FoundationId
           → DebtInfo → H.ComponentHTML Query
inputFDebt debtType cur myId friends debtInfo =
  let sendMsg  = case debtType of Debt   → "I Owe This"
                                  Credit → "I Am Owed This"
  in HH.div [ HP.class_ $ HH.ClassName $ "create-debt col "]
     [
       HH.label_ [ HH.text $ "Enter " <> show debtType ],
       HH.div [ HP.class_ $ HH.ClassName "row amount-row" ]
       [
         HH.span_ [HH.text $ F.cSymbol cur]
       , HH.input [ HP.type_ HP.InputNumber
                  , HP.class_ $ HH.ClassName "debt-amount"
                  , HP.value debtInfo.amount
                  , HP.step $ Step 0.01
                  , HE.onValueInput (HE.input (\v → InputDebtDetails debtType $
                                                    debtInfo {amount = v}))
                  ]
       , HH.span_ [HH.text $ F.cIsoCode cur]
       ]
     , HH.select [ HE.onValueChange
                   (HE.input (\v → if v == "Select a Friend"
                                   then InputDebtDetails debtType $
                                        debtInfo { counterParty = "" }
                                   else InputDebtDetails debtType $
                                        debtInfo { counterParty = v }))
                 ]
       ([ HH.option [ HP.selected true ] [ HH.text "Select a Friend"] ]
        <>
        ((\f → HH.option_ [ HH.text $ F.fiGetId f ]) <$> friends))
     , HH.input [ HP.type_ HP.InputText
                , HP.placeholder $ "Enter debt memo here"
                , HP.attr (HH.AttrName "maxlength") "32"
                , HE.onValueInput
                  (HE.input (\val → InputDebtDetails debtType $
                                    debtInfo { desc = S.take 32 val}))
                , HP.value $ S.take 32 debtInfo.desc ]
     , HH.button [ HE.onClick $ HE.input_ $ AddDebt debtType $
                   mkDebt debtType cur myId debtInfo
                 , HP.disabled $
                   (isNothing $ F.moneyFromDecString cur debtInfo.amount)
                   ||
                   (hasBlanks debtInfo)
                 , HP.class_ $ HH.ClassName "create-debt-button form-control"]
       [ HH.text $ sendMsg ]
     ]
  where hasBlanks di = di.amount == "" || di.desc == "" || di.counterParty == ""
        getDebtor debtType me friend = case debtType of
          Debt   → me
          Credit → friend
        getCreditor debtType me friend = case debtType of
          Debt   → friend
          Credit → me
        mkDebt debtType currency me debtInfo =
          F.mkDebt
          (getDebtor   debtType me (F.fiMkId debtInfo.counterParty))
          (getCreditor debtType me (F.fiMkId debtInfo.counterParty))
          (F.fiMkId debtInfo.counterParty)
          (fromMaybe (F.mkMoney 0.0 currency)
           (F.moneyFromDecString currency debtInfo.amount))
          F.NoDebtId
          debtInfo.desc

inputDebt   = inputFDebt Debt
inputCredit = inputFDebt Credit

numberFromString ∷ String → Number
numberFromString s = fromMaybe 0.0 (N.fromString s)
