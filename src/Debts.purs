module Debts where


import FriendInDebt.Prelude
import Types (FIDMonad, ContainerMsgBus, ContainerMsg(..), NameMap, DebtsMap)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Aff (Aff)
import Data.Array (singleton, head)
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
import UI.IconGenerator as ICON

data Query a
  = RefreshDebts a
  | HandleInput Input a
  | InputDebt F.Debt a
  | InputCredit F.Debt a
  | AddDebt F.Debt a
  | ConfirmPending F.Debt a
  | RejectPending F.Debt a
  | AddFriend (Either String F.FoundationId) a
  | InputFriend String a
  | InputName String a
  | UpdateName String a
  | ShowItemizedDebtFor (Maybe F.FoundationId) a

type Input = ContainerMsgBus
type Message = String

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
          $ displayFriendLi <$> (zip state.friends state.gradients)
        ]
      , HH.div
        [ HP.class_ $ HH.ClassName "all-balances-container" ]
        [
          HH.ul
          [ HP.class_ $ HH.ClassName "col-12" ]
          $ (displayBalanceLi state.myId) <$> [mockBalance, mockBalance, mockBalance, mockBalance, mockBalance, mockBalance]
        ]
      , HH.div
        [ HP.class_ $ HH.ClassName "all-pending-debts-container" ]
        [
          displaySentFriendsList state.pendingFriendsSent
        , displayTodoFriendsList state.pendingFriendsTodo
        , (displaySentDebtsList state.myId) state.pendingSent
        , (displayTodoList state.myId) state.pendingTodo
        ]
      , HH.div
        [ HP.class_ $ HH.ClassName "all-settings-container" ]
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
        , HH.ul_ [
            HH.li [ HP.class_ $ HH.ClassName "row create-debt-card" ]
            [inputDebt state.defaultCurrency state.myId state.friends state.newDebt]
        , HH.li [ HP.class_ $ HH.ClassName "row create-debt-card" ]
            [inputCredit state.defaultCurrency state.myId state.friends state.newCredit]
        ]
        ]
      ]

  eval ∷ Query ~> H.ComponentDSL State Query Message (FIDMonad eff)
  eval = case _ of
    ShowItemizedDebtFor maybeFriend next → do
      H.raise "show-itemized-debt"

      case maybeFriend of
        Nothing → pure next
        Just f  → do
          s ← H.get
          H.modify (_ { showItemizedDebtFor = maybeFriend, loading = true })
          idebts ← handleFIDCall s.errorBus [] (F.itemizedDebts f)
          H.modify (_ { loading = false
                      , itemizedDebts = M.insert f idebts s.itemizedDebts })
          pure next
    HandleInput input next → do
      state <- H.get
      -- gradients <- H.liftEff $ sequence $ ICON.randomGradient <$> state.friends
      gradients <- H.liftEff $ sequence $ (const ICON.randomGradient) <$> mockFriends
      H.modify (_ { errorBus = input, gradients = gradients, friends = mockFriends })
      -- H.modify (_ { errorBus = input, gradients = gradients })
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
    InputDebt debt next → do
      hLog debt
      H.modify (_ { newDebt = Just debt })
      pure next
    InputCredit credit next → do
      H.modify (_ { newCredit = Just credit })
      pure next
    AddDebt debt next → do
      hLog debt
      s ← H.get
      handleFIDCall s.errorBus unit (F.newPendingDebt debt)
      H.modify (_ { newDebt = Nothing, newCredit = Nothing })
      pure next
    ConfirmPending debt next → do
      s ← H.get
      handleFIDCall s.errorBus unit (F.confirmPendingDebt debt)
      pure next
    RejectPending debt next → do
      s ← H.get
      handleFIDCall s.errorBus unit (F.rejectPendingDebt debt)
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
  balances       ← handleFIDCall errorBus [] F.debtBalances
  H.modify (_ { myId = myId, friends = friends
              , pendingFriendsSent = F.pfGetSents pendingFriends
              , pendingFriendsTodo = F.pfGetTodos pendingFriends
              , pendingSent = F.pdGetSents pendingD
              , pendingTodo = F.pdGetTodos pendingD
              , balances = balances
              , loading = false
              })

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
            $ itemizedDebtLi <$> fakeDebts
        ]
        -- HH.ul_ $ itemizedDebtLi <$> (fromMaybe [] $ M.lookup f debtsMap)]
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

-- displayFriendLi ∷ Tuple(F.FoundationId ICON.GradientCss) → H.ComponentHTML Query
displayFriendLi (Tuple fid gradient) =
  HH.li [HP.class_ $ HH.ClassName "friend-row row"]
  [HH.a [HP.href "#", HE.onClick $ HE.input_ $ ShowItemizedDebtFor $ Just fid]
        [ICON.generatedIcon (show fid) gradient, HH.text $ show fid]]

-- Balance List

displayBalanceLi :: State → F.Balance → H.ComponentHTML Query
displayBalanceLi state (F.Balance bal) =
  let debtsMap = state.itemizedDebts
      me       = state.myId
      curFriend= if bal.creditor == me then bal.debtor else bal.creditor
      status   = if bal.creditor == me then "Holds debts from:" else "Is owing..."
      friendToShow = state.showItemizedDebtFor
      expandClass = (\f → if f == curFriend then "expand-itemized" else "hide-itemized") <$> friendToShow
  in
    HH.li [HP.class_ $ HH.ClassName $ "balance-row row " <> fromMaybe "" expandClass,
           HE.onClick $ HE.input_ $ ShowItemizedDebtFor $ Just bal.creditor]
    $ [
      HH.div [HP.class_ $ HH.ClassName "highlight"][],
      HH.div [HP.class_ $ HH.ClassName "col-4 debt-excerpt"][
        HH.div [HP.class_ $ HH.ClassName "row debt-amount"][moneySpan bal.amount],
        HH.div [HP.class_ $ HH.ClassName "row label-row"][HH.small_[HH.text "last"]],
        HH.div [HP.class_ $ HH.ClassName "row thin-item-row"]
          [
            HH.span [HP.class_ $ HH.ClassName "thin-item"][HH.text "2017/07/01"]
          ]
      ],
      HH.div [HP.class_ $ HH.ClassName "col debt-details"][
        HH.div [HP.class_ $ HH.ClassName "col debt-relationship"][
          HH.div [HP.class_ $ HH.ClassName "row"][HH.text status],
          HH.div [HP.class_ $ HH.ClassName "row"][HH.h6_ [HH.text $ show bal.creditor]]
        ],
        HH.div [HP.class_ $ HH.ClassName "row label-row"][
          HH.div [HP.class_ $ HH.ClassName "col-6"][HH.small_[HH.text "currency"]],
          HH.div [HP.class_ $ HH.ClassName "col-6"][HH.small_[HH.text "debts"]]
        ],
        HH.div [HP.class_ $ HH.ClassName "row thin-item-row"][
          HH.div [HP.class_ $ HH.ClassName "col thin-item"][currencySpan bal.amount],
          HH.div [HP.class_ $ HH.ClassName "col thin-item"][HH.text $ "314" <> "debts"]
        ]
      ],
      (displayItemizedDebtTimeline friendToShow debtsMap curFriend)
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
    false → HH.a [HP.class_ $ HH.ClassName "expandable-id", HP.href "#", HE.onClick $ HE.input_ $ ShowItemizedDebtFor $ Just idToDisplay] [HH.text $ show idToDisplay]

descSpan ∷ F.Debt → H.ComponentHTML Query
descSpan (F.Debt fd) =
  HH.span [] [ HH.text $ fd.desc ]

currencySpan ∷ F.Money → H.ComponentHTML Query
currencySpan (F.Money m) =
  HH.span [HP.class_ $ HH.ClassName "currency-span"] [ HH.text $ show m.currency ]

moneySpan ∷ F.Money → H.ComponentHTML Query
moneySpan (F.Money m) =
  HH.span [HP.class_ $ HH.ClassName "money-span"] [ HH.text $ "$" <> (show $ m.amount) ]

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
confirmButton fd = HH.button [ HP.class_ $ HH.ClassName "btn-confirm"
                             , HE.onClick $ HE.input_ $ ConfirmPending fd]
  [ HH.text "Confirm" ]

cancelButton ∷ F.Debt → H.ComponentHTML Query
cancelButton fd = HH.button [ HP.class_ $ HH.ClassName "btn-cancel"
                             , HE.onClick $ HE.input_ $ RejectPending fd]
  [ HH.text "Cancel" ]

confirmFriendshipButton :: F.FoundationId -> H.ComponentHTML Query
confirmFriendshipButton friend =
  HH.button [ HE.onClick $ HE.input_ $ AddFriend $ Right friend
            , HP.class_ $ HH.ClassName "confirm-friend-button"]
            [ HH.text "Confirm Friendship" ]

addFriendWidget ∷ State → H.ComponentHTML Query
addFriendWidget state =
  HH.div [ HP.class_ $ HH.ClassName "addFriend" ]
  [
    HH.small_ [HH.text "Friend's FoundationID"],
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
nonZero fd = ((F.numAmount ∘ F.debtMoney) fd) /= (toNumber 0)

data DebtType = Debt | Credit
inputFDebt ∷ DebtType → F.Currency → F.FoundationId → Array F.FoundationId
          → Maybe F.Debt → H.ComponentHTML Query
inputFDebt debtType cur myId friends maybeDebt =
  case head friends of
    Nothing       → HH.div_ [ HH.text "No friends to debt" ]
    Just friendId →
      let d = case debtType of
            Debt   → fromMaybe (F.zeroDebt cur myId friendId friendId) maybeDebt
            Credit → fromMaybe (F.zeroDebt cur friendId myId friendId) maybeDebt
          handler = case debtType of Debt   → InputDebt
                                     Credit → InputCredit
      in HH.div [ HP.class_ $ HH.ClassName "createDebt col row" ]
         [
           HH.input [ HP.type_ HP.InputNumber
                    , HP.class_ $ HH.ClassName "debt-amount col-2"
                    , HP.value $ noDecimals $ F.formatMoney $ F.debtMoney d
                    , HE.onValueInput
                      (HE.input (\val → handler $ amount d val cur))
                    , HP.min $ toNumber (-1000000)
                    , HP.max $ toNumber 1000000]
         , HH.select [ HE.onValueChange
                       (HE.input (\v → handler $ counterparty d debtType v))
                     ]
             ((\f → HH.option_ [ HH.text $ F.fiGetId f ]) <$> friends)
         , HH.input [ HP.type_ HP.InputText
                    , HP.placeholder $ "Enter debt memo here"
                    , HE.onValueInput
                      (HE.input (\val → handler $ F.setDesc d (S.take 32 val)))
                    , HP.value $ S.take 32 $ F.getDesc d ]
         , HH.button [ HE.onClick $ HE.input_ $ AddDebt d
                     , HP.disabled $ F.debtAmount d == 0.0
                     , HP.class_ $ HH.ClassName "create-debt-button col-2"]
           [ HH.text $ "Send Debt" ]
         ]
      where noDecimals = S.takeWhile (\c → c /= '.')
            amount debt v currency = F.setDebtMoney debt $
              F.moneyFromDecString (noDecimals v) currency
            counterparty debt dType v = case dType of
              Debt   → F.debtSetCreditor debt (F.fiMkId v)
              Credit → F.debtSetDebtor   debt (F.fiMkId v)

inputDebt   = inputFDebt Debt
inputCredit = inputFDebt Credit

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
mockFriends = [F.FoundationId "jaredbowie", F.FoundationId "TimTime", F.FoundationId "chinmich", F.FoundationId "tom", F.FoundationId "aki", F.FoundationId "brad"]

mockNameMap :: NameMap
mockNameMap = M.insert (F.FoundationId "bob") "Bob Brown" $ M.empty

fakeDebt :: F.Debt
fakeDebt = mockDebt $ F.FoundationId "bob"

fakeDebts :: Array F.Debt
fakeDebts = [fakeDebt, fakeDebt]

mockMe :: F.FoundationId
mockMe = (F.FoundationId "lukezhang")

fakeFriend :: F.FoundationId
fakeFriend = (F.FoundationId "jaredbowie")

fakeFriend2 :: F.FoundationId
fakeFriend2 = (F.FoundationId "timtime")
--
--
-- mockDebtMap :: DebtsMap
-- mockDebtMap = M.insert (F.FoundationId "bob") fakeDebt $ M.empty

mockBalance :: F.Balance
mockBalance = F.Balance { debtor: mockMe, creditor: fakeFriend, amount: F.Money {amount: 5.0, currency: F.cUSD}}

mockBalance2 :: F.Balance
mockBalance2 = F.Balance { debtor: mockMe, creditor: fakeFriend2, amount: F.Money {amount: 15.0, currency: F.cUSD}}


mockPendingDebts :: F.PendingDebts
mockPendingDebts = F.PD {sent: [fakeDebt], todo: [fakeDebt]}


mockFoundationId :: F.FoundationId
mockFoundationId = F.FoundationId "snoopy"
mockDebt :: F.FoundationId -> F.Debt
mockDebt fid = F.mkDebt mockFoundationId fid fid (F.moneyFromDecString "2.0" F.cUSD) F.NoDebtId "Dinner @ KRBB"
