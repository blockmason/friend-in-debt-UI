module Debts where


import FriendInDebt.Prelude
import FriendInDebt.Types (FIDMonad, ContainerMsgBus, ContainerMsg(..), NameMap, DebtsMap, InputMoney)
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

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import FriendInDebt.Blockchain          (handleCall, handleTx, hasNetworkError, formatDate)
import Network.Eth.FriendInDebt         as F
import Network.Eth                      as E
import UI.IconGenerator as ICON

import FriendInDebt.Routes              as R

data Query a
  = RefreshDebts a
  | HandleInput Input a
  | InputDebtAmount DebtType InputMoney a
  | InputDebtDetails DebtType F.Debt a
  | AddDebt F.Debt a
  | ConfirmPending F.Debt a
  | RejectPending F.Debt a
  | AddFriend (Either String F.FoundationId) a
  | InputFriend String a
  | ShowItemizedDebtFor (Maybe F.FoundationId) a

type Input = ContainerMsgBus

data Message
  = ScreenChange R.Screen
  | NewTX E.TX
newtype FriendBundle = FriendBundle { id ∷ F.FoundationId, gradient ∷ ICON.GradientCss, balance ∷ Maybe F.Balance }

type State = { friends             ∷ Array F.FoundationId
             , gradients           ∷ Array ICON.GradientCss
             , pendingFriendsTodo  ∷ Array F.FoundationId
             , pendingFriendsSent  ∷ Array F.FoundationId
             , balances            ∷ Array F.Balance
             , itemizedDebts       ∷ DebtsMap
             , myId                ∷ F.FoundationId
             , pendingSent         ∷ Array F.Debt
             , pendingTodo         ∷ Array F.Debt
             , newDebt             ∷ Maybe F.Debt
             , newCredit           ∷ Maybe F.Debt
             , newFriend           ∷ Either String F.FoundationId
             , userName            ∷ Either F.FoundationId F.UserName
             , inputName           ∷ String
             , showItemizedDebtFor ∷ Maybe F.FoundationId
             , defaultCurrency     ∷ F.Currency
             , loading             ∷ Boolean
             , inputChanged        ∷ Boolean
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
                       , newFriend: Left ""
                       , newDebt: Nothing
                       , newCredit: Nothing
                       , userName: (Right "")
                       , inputName: ""
                       , showItemizedDebtFor: Nothing
                       , defaultCurrency: F.cUSD
                       , loading: false
                       , inputChanged: false
                       , errorBus: input }

  render ∷ State → H.ComponentHTML Query
  render state =
    -- if state.loading
    -- then HH.span_ [ HH.h6_ [ HH.text "Loading debt info..." ]
    --               , HH.img [ HP.src "loading.gif"
    --                        , HP.width 25 ] ]
    -- else
      HH.div
      [ HP.class_ $ HH.ClassName "page-container col-12" ]
      [
        page R.FriendsScreen $
             HH.ul [ HP.class_ $ HH.ClassName "col" ]
             $ groupFriendLiByInitial state.defaultCurrency $ prepareFriendBundles state

      , page R.BalancesScreen $
             HH.ul
             [ HP.class_ $ HH.ClassName "col-12" ]
             $ (displayBalanceLi state) <$> state.balances
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
    ShowItemizedDebtFor maybeFriend next → do
      H.raise $ ScreenChange R.ItemizedDebtsScreen

      case maybeFriend of
        Nothing → pure next
        Just f  → do
          s ← H.get
          H.modify (_ { loading = true, showItemizedDebtFor = maybeFriend })
          idebts ← handleCall s.errorBus [] (F.itemizedDebts f)
          H.modify (_ { loading = false
                      , itemizedDebts = M.insert f idebts s.itemizedDebts })
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
    AddFriend eitherFriendId next → do
      s ← H.get
      hLog eitherFriendId
      case eitherFriendId of
        Left  str → pure next
        Right friendId → do
          H.modify (_ { newFriend = Left "" })
          handleTx NewTX s (ScreenChange R.BalancesScreen) $ F.createFriendship friendId
          pure next
    InputFriend friendStr next → do
      if ((S.length friendStr) > 3) --id should be longer than 3 characters
        then H.modify (_ { newFriend = Right $ F.FoundationId friendStr })
        else H.modify (_ { newFriend = Left friendStr })
      pure next
    InputDebtAmount debtType im next → do
      c  ← H.gets _.defaultCurrency
      let m  = F.moneyFromDecString c $ show im.whole <> "." <> show im.decs
      case debtType of
        Debt   → H.modify (_ { newDebt   = Just $ F.setDebtMoney im.debt m })
        Credit → H.modify (_ { newCredit = Just $ F.setDebtMoney im.debt m })
      H.modify (\s → s { inputChanged = not s.inputChanged })
      case debtType of
        Debt   → (H.gets _.newDebt)   >>= hLog
        Credit → (H.gets _.newCredit) >>= hLog
      pure next
    InputDebtDetails debtType debt next → do
      case debtType of
        Debt   → H.modify (_ { newDebt   = Just debt })
        Credit → H.modify (_ { newCredit = Just debt })
      ic ← H.gets _.inputChanged
      H.modify (\s → s { inputChanged = not s.inputChanged })
      hLog debt
      pure next
    AddDebt debt next → do
      H.modify (_ { loading = true })
      hLog debt
      s ← H.get
      handleTx NewTX s (ScreenChange R.BalancesScreen) $ F.newPendingDebt debt
      H.modify (_ { newDebt = Nothing, newCredit = Nothing, loading = false })
      pure next
    ConfirmPending debt next → do
      s ← H.get
      handleTx NewTX s (ScreenChange R.BalancesScreen) $ F.confirmPendingDebt debt
      pure next
    RejectPending debt next → do
      s ← H.get
      handleTx NewTX s (ScreenChange R.BalancesScreen) $ F.rejectPendingDebt debt
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
              , loading = false
              })

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
          [inputDebt state.inputChanged state.defaultCurrency state.myId state.friends state.newDebt]
      , HH.li [ HP.class_ $ HH.ClassName "row create-debt-card" ]
          [inputCredit state.inputChanged state.defaultCurrency state.myId state.friends state.newCredit]
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
displayItemizedDebtTimeline friendToShow debtsMap curFriend =
  case friendToShow of
    Just f →
      HH.div
        [ HP.class_ $ HH.ClassName $ "col itemized-debts"]
        [
          HH.div [HP.class_ $ HH.ClassName "row"][HH.text "Debt History:"],
          HH.div [HP.class_ $ HH.ClassName "row"][HH.h6_ [HH.text $ show f]],
          HH.ul [HP.class_ $ HH.ClassName "row debt-timeline"]
            $ itemizedDebtLi <$> (fromMaybe [] $ M.lookup f debtsMap)
        ]
    Nothing → HH.div_ [ HH.text "" ]

itemizedDebtLi ∷ F.Debt → H.ComponentHTML Query
itemizedDebtLi fd =
  HH.li [HP.class_ $ HH.ClassName "timeline-event"] $ itemizedDebt fd

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

groupFriendLiByInitial ∷ F.Currency → Array FriendBundle → Array (H.ComponentHTML Query)
groupFriendLiByInitial c friendBundles =
  let
    orderedFriends = sortBy (\(FriendBundle bundle1) (FriendBundle bundle2) → S.localeCompare (F.initial bundle1.id) (F.initial bundle2.id)) friendBundles
    friendGroups = groupBy (\(FriendBundle bundle1) (FriendBundle bundle2) → (F.initial bundle1.id) == (F.initial bundle2.id)) orderedFriends
  in
    displayFriendGroup c <$> friendGroups

displayFriendGroup ∷ F.Currency → NonEmpty Array FriendBundle → H.ComponentHTML Query
displayFriendGroup c group =
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
                     $ displayFriendLi c <$> innerArr
            ]

displayFriendLi ∷ F.Currency → FriendBundle → H.ComponentHTML Query
displayFriendLi c (FriendBundle bundle) =
  let amount = maybe (F.mkMoney 0.0 c) F.balAmount bundle.balance
  in HH.li [HP.class_ $ HH.ClassName "friend-item row"]
    [
      HH.div [HP.class_ $ HH.ClassName "col-3"]
        [ICON.generatedIcon (show bundle.id) bundle.gradient]
      , HH.div [HP.class_ $ HH.ClassName "col-9 name-portion"]
        [
          HH.text $ show bundle.id,
          HH.span_ [HH.text $ "Balance: " <> F.formatMoney amount ]
        ]
    ]

-- Balance List

displayBalanceLi :: State → F.Balance → H.ComponentHTML Query
displayBalanceLi state bal =
  let debtsMap = state.itemizedDebts
      me       = state.myId
      creditor = F.balCreditor bal
      debtor   = F.balCreditor bal
      amount   = F.balAmount bal
      totalDebts  = F.balTotalDebts bal
      mostRecent  = maybe "" formatDate $ F.balMostRecent bal
      curFriend = if creditor == me then debtor else creditor
      status    = if creditor == me then "Holds debts from:" else "Is owing..."
      friendToShow = state.showItemizedDebtFor
      expandClass = (\f → if f == curFriend then "expand-itemized" else "hide-itemized") <$> friendToShow
  in
    HH.li [HP.class_ $ HH.ClassName $ "balance-row row " <> fromMaybe "" expandClass,
           HE.onClick $ HE.input_ $ ShowItemizedDebtFor $ Just curFriend]
    $ [
      HH.div [HP.class_ $ HH.ClassName "highlight"][],
      HH.div [HP.class_ $ HH.ClassName "col-4 debt-excerpt"][
        HH.div [HP.class_ $ HH.ClassName "row debt-amount"][moneySpan amount],
        HH.div [HP.class_ $ HH.ClassName "row label-row"][HH.small_[HH.text "last"]],
        HH.div [HP.class_ $ HH.ClassName "row thin-item-row"]
          [
            HH.span [HP.class_ $ HH.ClassName "thin-item"]
            [HH.text mostRecent]
          ]
      ],
      HH.div [HP.class_ $ HH.ClassName "col debt-details"][
        HH.div [HP.class_ $ HH.ClassName "col debt-relationship"][
          HH.div [HP.class_ $ HH.ClassName "row"][HH.text status],
          HH.div [HP.class_ $ HH.ClassName "row"][HH.h6_ [HH.text $ show debtor]]
        ],
        HH.div [HP.class_ $ HH.ClassName "row label-row"][
          HH.div [HP.class_ $ HH.ClassName "col-6"][HH.small_[HH.text "currency"]],
          HH.div [HP.class_ $ HH.ClassName "col-6"][HH.small_[HH.text "debts"]]
        ],
        HH.div [HP.class_ $ HH.ClassName "row thin-item-row"][
          HH.div [HP.class_ $ HH.ClassName "col thin-item"][currencySpan amount],
          HH.div [HP.class_ $ HH.ClassName "col thin-item"][HH.text $ show totalDebts <> " debts"]
        ]
      ],
      (displayItemizedDebtTimeline friendToShow debtsMap curFriend)
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
      HH.div [HP.class_ $ HH.ClassName "col friend-details"]
      [
        HH.div [HP.class_ $ HH.ClassName "request-details"]
        [
          HH.span_ [HH.text "Friend Request To"],
          HH.span_ [HH.text $ show friend],
          HH.span_ [HH.text "2017/07/29 15:35"]
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
      HH.div [HP.class_ $ HH.ClassName "col-1"][confirmFriendshipButton friend]
    ]
  ]

displayTodoFriend ::  F.FoundationId → Array (H.ComponentHTML Query)
displayTodoFriend friend =
    [
      HH.div [HP.class_ $ HH.ClassName "col friend-details"]
      [
        HH.div [HP.class_ $ HH.ClassName "request-details"]
        [
          HH.span_ [HH.text "Friend Request From"],
          HH.span_ [HH.text $ show friend],
          HH.span_ [HH.text "2017/07/29 15:35"]
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
        HH.div [HP.class_ $ HH.ClassName "debtor"][idSpan me fd'.debtor],
        HH.div [HP.class_ $ HH.ClassName "creditor"][idSpan me fd'.creditor],
        HH.div [HP.class_ $ HH.ClassName "desc"][descSpan fd],
        HH.div [HP.class_ $ HH.ClassName "amount"][debtAmountSpan fd]
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
    HH.div [HP.class_ $ HH.ClassName "row action-buttons row align-items-center"][
      HH.div [HP.class_ $ HH.ClassName "col-1"][cancelButton fd],
      HH.div [HP.class_ $ HH.ClassName "col-1"][confirmButton fd]
    ]
  ]

displayTodo ::  F.FoundationId → F.Debt → Array (H.ComponentHTML Query)
displayTodo me fd =
  let (F.Debt fd') = fd
  in
    [
      HH.div [HP.class_ $ HH.ClassName "row debt-details row align-items-center"]
      [
        HH.div [HP.class_ $ HH.ClassName "debtor"][idSpan me fd'.debtor],
        HH.div [HP.class_ $ HH.ClassName "creditor"][idSpan me fd'.creditor],
        HH.div [HP.class_ $ HH.ClassName "desc"][descSpan fd],
        HH.div [HP.class_ $ HH.ClassName "amount"][debtAmountSpan fd]
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
confirmButton fd = HH.button [ HP.class_ $ HH.ClassName "fa fa-check"
                             , HE.onClick $ HE.input_ $ ConfirmPending fd]
                             []

cancelButton ∷ F.Debt → H.ComponentHTML Query
cancelButton fd = HH.button [ HP.class_ $ HH.ClassName "fa fa-close"
                             , HE.onClick $ HE.input_ $ RejectPending fd]
                            []

confirmFriendshipButton :: F.FoundationId -> H.ComponentHTML Query
confirmFriendshipButton friend =
  HH.button [ HE.onClick $ HE.input_ $ AddFriend $ Right friend
            , HP.class_ $ HH.ClassName "confirm-friend-button"]
            [ HH.i [HP.class_ $ HH.ClassName "fa fa-check"][] ]

addFriendWidget ∷ State → H.ComponentHTML Query
addFriendWidget state =
  HH.div [ HP.class_ $ HH.ClassName "add-friend" ]
  [
    HH.h6 [ HP.class_ $ HH.ClassName "modal-title"][HH.text "Add New Friend"],
    HH.label [][HH.text "Friend's FoundationID"],
    HH.input [ HP.type_ HP.InputText
             , HP.value $ inputVal state.newFriend
             , HP.class_ $ HH.ClassName "form-control"
             , HP.placeholder $ "johndoe"
             , HE.onValueInput
               (HE.input (\val → InputFriend val))
             ]
  , HH.button [ HE.onClick $ HE.input_ $ AddFriend state.newFriend
              , HP.class_ $ HH.ClassName "form-control"]
    [ HH.text "Add Friend by FoundationId" ]
  ]
  where inputVal = either id show

nonZero ∷ F.Debt → Boolean
nonZero fd = ((F.numAmount ∘ F.debtMoney) fd) /= 0.0

instance showDebtType :: Show DebtType where
show debtType =
  case debtType of
    Debt → "Debt You Owe"
    Credit → "Debt Owed To You"

data DebtType = Debt | Credit
--Boolean passed is just to force a refresh
inputFDebt ∷ DebtType → Boolean → F.Currency → F.FoundationId → Array F.FoundationId
          → Maybe F.Debt → H.ComponentHTML Query
inputFDebt debtType _ cur myId friends maybeDebt =
  case head friends of
    Nothing       → HH.div_ []
    Just friendId →
      let d = case debtType of
            Debt   → fromMaybe (F.zeroDebt cur myId friendId friendId) maybeDebt
            Credit → fromMaybe (F.zeroDebt cur friendId myId friendId) maybeDebt
          sendMsg  = case debtType of Debt   → "I Owe This"
                                      Credit → "I Am Owed This"
          showCents = case (F.cDecimals cur) of 0 → ""
                                                _ → "show-cents"
      in HH.div [ HP.class_ $ HH.ClassName $ "create-debt col " <> showCents]
         [
           HH.label_ [ HH.text $ "Enter " <> show debtType ],
           HH.div [ HP.class_ $ HH.ClassName "row amount-row" ]
            [
               HH.span_ [HH.text $ F.cSymbol cur]
             , HH.input [ HP.type_ HP.InputNumber
                        , HP.class_ $ HH.ClassName "debt-amount"
                        , HP.value $ show $ whole d
                        , HE.onValueInput
                          (HE.input (\v → InputDebtAmount debtType
                                          { whole: fromMaybe 0 $ I.fromString v
                                          , decs:  decs d
                                          , debt: d }))
                        ]
             , HH.span_ [HH.text "."]
             , HH.input [ HP.type_ HP.InputNumber
                        , HP.class_ $ HH.ClassName "debt-amount"
                        , HP.value $ show $ decs d
                        , HE.onValueInput
                          (HE.input (\v → InputDebtAmount debtType
                                          { whole: whole d
                                          , decs:  fromMaybe 0 $ I.fromString $ S.take 2 v
                                          , debt: d }))
                        ]
             , HH.span_ [HH.text $ F.cIsoCode cur]
            ]
         , HH.select [ HE.onValueChange
                       (HE.input (\v → InputDebtDetails debtType $ counterparty d debtType v))
                     ]
             ((\f → HH.option_ [ HH.text $ F.fiGetId f ]) <$> friends)
         , HH.input [ HP.type_ HP.InputText
                    , HP.placeholder $ "Enter debt memo here"
                    , HE.onValueInput
                      (HE.input (\val → InputDebtDetails debtType $ F.setDesc d (S.take 32 val)))
                    , HP.value $ S.take 32 $ F.getDesc d ]
         , HH.button [ HE.onClick $ HE.input_ $ AddDebt d
                     , HP.disabled $ F.debtAmount d == 0.0
                     , HP.class_ $ HH.ClassName "create-debt-button form-control"]
           [ HH.text $ sendMsg ]
         ]
      where whole ∷ F.Debt → Int
            whole = F.moneyWhole ∘ F.debtMoney
            decs  ∷ F.Debt → Int
            decs  = F.moneyDecimals ∘ F.debtMoney
            counterparty debt dType v = case dType of
              Debt   → F.debtSetCreditor debt (F.fiMkId v)
              Credit → F.debtSetDebtor   debt (F.fiMkId v)

inputDebt   = inputFDebt Debt
inputCredit = inputFDebt Credit

numberFromString ∷ String → Number
numberFromString s = fromMaybe 0.0 (N.fromString s)
--
-- mockFriendNames :: Array String
-- mockFriendNames = ["bob", "tim", "kevin"]
--
-- mockFriends :: Array F.FoundationId
-- mockFriends = [F.FoundationId "jaredbowie", F.FoundationId "TimTime", F.FoundationId "chinmich", F.FoundationId "tom", F.FoundationId "aki", F.FoundationId "brad"]
--
-- mockNameMap :: NameMap
-- mockNameMap = M.insert (F.FoundationId "bob") "Bob Brown" $ M.empty
--
-- fakeDebt :: F.Debt
-- fakeDebt = mockDebt $ F.FoundationId "bob"
--
-- fakeDebts :: Array F.Debt
-- fakeDebts = [fakeDebt, fakeDebt]
--
-- mockMe :: F.FoundationId
-- mockMe = (F.FoundationId "lukezhang")
--
-- fakeFriend :: F.FoundationId
-- fakeFriend = (F.FoundationId "jaredbowie")
--
-- fakeFriend2 :: F.FoundationId
-- fakeFriend2 = (F.FoundationId "timtime")
-- --
-- --
-- -- mockDebtMap :: DebtsMap
-- -- mockDebtMap = M.insert (F.FoundationId "bob") fakeDebt $ M.empty
--
-- mockBalance :: F.Balance
-- mockBalance = F.Balance { debtor: mockMe, creditor: fakeFriend, amount: F.Money {amount: 5.0, currency: F.cUSD}, totalDebts: 13, mostRecent: Nothing}
--
-- mockBalance2 :: F.Balance
-- mockBalance2 = F.Balance { debtor: mockMe, creditor: fakeFriend2, amount: F.Money {amount: 15.0, currency: F.cUSD}, totalDebts: 13, mostRecent: Nothing}
--
-- mockPendingDebts :: F.PendingDebts
-- mockPendingDebts = F.PD {sent: [fakeDebt], todo: [fakeDebt]}
--
-- mockFoundationId :: F.FoundationId
-- mockFoundationId = F.FoundationId "snoopy"
-- mockDebt :: F.FoundationId -> F.Debt
-- mockDebt fid = F.mkDebt mockFoundationId fid fid (F.moneyFromDecString "2.0" F.cUSD) F.NoDebtId "Dinner @ KRBB"
