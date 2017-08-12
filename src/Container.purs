module Container where

import FriendInDebt.Prelude

import FriendInDebt.Types (FIDMonad, ContainerMsgBus, ContainerMsg(..))
import Data.Either.Nested (Either1)
import Control.Monad.Eff.Console (logShow)
import Data.Functor.Coproduct.Nested (Coproduct1)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff (delay, launchAff)
import Control.Monad.Eff.Timer (setInterval)
import Data.Time.Duration (Milliseconds(..))
import Data.Int (toNumber)
import Data.Array as A

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Component.ChildPath as CP
import Halogen.Component.Utils (busEventSource)
import Halogen.Query.EventSource as ES

import Debts                    as D
import Network.Eth.Metamask     as MM
import Network.Eth              as E
import Network.Eth.FriendInDebt as F

import FriendInDebt.Routes      as R
import FriendInDebt.Config      as C
import FriendInDebt.Blockchain (hasNetworkError, handleCall)

import UI.UIStatesKit as UIStates

data Query a
  = Init a
  | HandleMsg ContainerMsg a
  | RefreshData a
  | SetScreen R.Screen a
  | PreviousScreen a
  | DebtViewMsg D.Message a

type State = { loggedIn ∷ Boolean
             , myId     ∷ Maybe F.FoundationId
             , myAddr   ∷ Maybe E.EthAddress
             , errorBus ∷ ContainerMsgBus
             , errorToDisplay ∷ Maybe ContainerMsg
             , txs      ∷ Array E.TX
             , numPendingTodo ∷ Int
             , numPendingFriends ∷ Int
             , currentScreen ∷ R.Screen
             , history  ∷ Array R.Screen
             , logText  ∷ Array String
             }

type ChildQuery = Coproduct1 D.Query
type ChildSlot = Either1 Unit

ui ∷ ∀ eff. H.Component HH.HTML Query Unit Void (FIDMonad eff)
ui =
  H.lifecycleParentComponent
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  , initializer: (Just (H.action Init))
  , finalizer: Nothing
  }
  where

    initialState ∷ State
    initialState = { loggedIn: false
                   , myId: Nothing
                   , myAddr: Nothing
                   , errorBus: Nothing
                   , errorToDisplay: Nothing
                   , txs: []
                   , numPendingTodo: 0
                   , numPendingFriends: 0
                   , currentScreen: R.BalancesScreen
                   , history: []
                   , logText: []
                   }

    render ∷ State → H.ParentHTML Query ChildQuery ChildSlot (FIDMonad eff)
    render state =
      HH.div [ HP.id_ "container",
               HP.class_ (HH.ClassName $
                 "container " <>
                 (R.getRouteNameFor state.currentScreen)) ]
      [
      errorOverlay state
      , topBar state
      , menu state
      , HH.div [ HP.class_ (HH.ClassName "create-debt-bar") ]
      [
        HH.a [HP.href "#", HP.class_ (HH.ClassName ""), HE.onClick $ HE.input_ $ SetScreen R.CreateDebtScreen] [
        HH.i [ HP.class_ (HH.ClassName "fa fa-plus")][], HH.text "Add New Debt"]
      ]
      , HH.div [ HP.class_ (HH.ClassName "add-friend-bar") ]
      [
        HH.a [HP.href "#", HP.class_ (HH.ClassName ""), HE.onClick $ HE.input_ $ SetScreen R.AddFriendScreen] [
        HH.i [ HP.class_ (HH.ClassName "fa fa-plus")][], HH.text "Add New Friend"]
      ]
      , HH.div [ HP.class_ (HH.ClassName "row main-view")]
        [
          HH.slot' CP.cp1 unit D.component state.errorBus $ HE.input DebtViewMsg
        ]
      ]

    eval ∷ Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (FIDMonad eff)
    eval = case _ of
      Init next → do
        hasWeb3 ← H.liftEff MM.hasWeb3
        if not hasWeb3 then H.modify (_ { errorToDisplay = Just CheckMetamask })
          else do
            netId ← H.liftAff MM.getNetwork
            if netId /= C.networkId
              then H.modify (_ { errorToDisplay = Just WrongEthNetwork })
              else do
                bus ← H.liftAff $ Bus.make
                H.liftEff $ UIStates.toggleLoading(".container")
                H.subscribe $ busEventSource (flip HandleMsg ES.Listening) bus
                H.modify (_ { loggedIn = true, errorBus = Just bus })
                loadWeb3Loop C.web3Delay 30
                startCheckInterval (Just bus) C.checkMMInterval C.checkTxInterval
        pure next
      HandleMsg msg next → do
        case msg of
          WrongEthNetwork → do
            H.modify (_ { errorToDisplay = Just WrongEthNetwork })
            pure next
          NetworkError → do
            hLog NetworkError
            H.liftEff $ UIStates.clearAllLoading Nothing
            H.modify (_ { loggedIn = false, errorToDisplay = Just NetworkError })
            pure next
          FIDError e → do
            case e of
              F.NoFoundationId → do
                H.modify (_ { myId = Nothing, errorToDisplay = Just (FIDError e) })
                pure next
              F.NetworkError → do
                hLog F.NetworkError
                H.liftEff $ UIStates.clearAllLoading Nothing
                H.modify (_ { loggedIn = false, errorToDisplay = Just (FIDError e)  })
                pure next
              F.TxError → do
                hLog "Transaction cancelled or error"
                pure next
              _ → do
                hLog e
                refreshData
                pure next
          CheckMetamask → do
            mmStatus ← H.liftAff MM.loggedIn
            loggedIn ← H.gets _.loggedIn
            checkMetamask loggedIn mmStatus
            pure next
          CheckTxs → do
            txs ← H.gets _.txs
            bus ← H.gets _.errorBus
            statii ← H.liftAff $ sequence (MM.checkTxStatus <$> txs)
            if hasNetworkError statii
              then case bus of
                Nothing → pure unit
                Just b  → H.liftAff $ Bus.write NetworkError b
              else do
                let pending = A.filter (\(Tuple s _) → E.notDone s) $ A.zip statii txs
                if A.length pending /= A.length txs
                  then do
                    H.modify (_ { txs = (\(Tuple _ tx) → tx) <$> pending })
                    if A.length pending == 0 then refreshData else pure unit
                  else pure unit
            pure next
      RefreshData next → do
        refreshData
        pure next
      SetScreen screen next → do
        H.modify (\state → state {history = append [state.currentScreen] state.history })
        H.modify (_ {currentScreen = screen})
        pure next
      DebtViewMsg msg next → do
        case msg of
          D.ScreenChange screen →
            H.modify (\s → s { history = append [s.currentScreen] s.history
                             , currentScreen = screen })
          D.NewTX newTx →
            H.modify (\s → s { txs = s.txs <> [newTx] })
          D.NumPendingTodo n →
            H.modify (_ { numPendingTodo = n })
          D.NumPendingFriends n →
            H.modify (_ { numPendingFriends = n })
          D.LoadId fid →
            H.modify (_ { myId = Just fid })
        pure next
      PreviousScreen next → do
        H.modify (\state → state {currentScreen = (fromMaybe R.BalancesScreen $ A.head state.history), history = (fromMaybe [] $ A.tail state.history)})
        pure next

errorOverlay ∷ ∀ p. State → H.HTML p Query
errorOverlay state =
  case state.errorToDisplay of
    Nothing →
      HH.div [ HP.id_ "no-errors"][]

    Just WrongEthNetwork →
      HH.div [ HP.id_ "errorOverlay"]
      [ HH.h6_ [ HH.text $ "Connected to wrong Ethereum network. Please switch your client to use Ropsten Test Network."]
      , HH.button [ HE.onClick $ HE.input_ $ RefreshData
                  , HP.class_ $ HH.ClassName "error-action"]
        [ HH.i [HP.class_ (HH.ClassName "fa fa-refresh")][] ]]

    Just CheckMetamask →
      HH.div [ HP.id_ "errorOverlay"][
        HH.h6_ [ HH.text $ "Can't connect to Metamask. Check Metamask or internet connection."],
        HH.button [ HE.onClick $ HE.input_ $ RefreshData
                     , HP.class_ $ HH.ClassName "error-action"]
           [ HH.i [HP.class_ (HH.ClassName "fa fa-refresh")][] ]
      ]

    Just (FIDError e) →
        case e of
          F.NoFoundationId →
            HH.div [ HP.id_ "errorOverlay"][
              HH.h6_ [ HH.text "No Foundation Id Detected"],
              HH.button [ HE.onClick $ HE.input_ $ RefreshData
                          , HP.class_ $ HH.ClassName "error-action"]
                [ HH.i [HP.class_ (HH.ClassName "fa fa-user-plus")][], HH.text "Register" ]
            ]
          F.NetworkError →
            HH.div [ HP.class_ (HH.ClassName "row error-notification")]
            [HH.text "Ropsten Test Network failing to respond to transaction... "]
          _ →
            HH.div [ HP.id_ "no-errors"][]

    Just genericError →
      HH.div [ HP.class_ (HH.ClassName "row error-notification")]
      [HH.text $ show genericError]

refreshData ∷ ∀ e. H.ParentDSL State Query ChildQuery ChildSlot Void (FIDMonad e) Unit
refreshData = do
  H.liftEff $ UIStates.turnOnLoading(".container")
  mmStatus ← H.liftAff MM.loggedIn
  myAddr   ← H.liftAff MM.currentUserAddress
  H.modify (_ { myAddr = Just myAddr })
  if mmStatus
    then do _ ← H.query' CP.cp1 unit (D.RefreshDebts unit)
            newmmStatus ← H.liftAff MM.loggedIn
            currentError ← H.gets _.errorToDisplay
            let errorToDisplay = if newmmStatus
                                 then
                                   case currentError of
                                     Just CheckMetamask → Nothing
                                     e                  → e
                                 else Just CheckMetamask
            H.modify (_ { loggedIn = newmmStatus, errorToDisplay = errorToDisplay })
    else do H.modify (_ { loggedIn = false, errorToDisplay = Just CheckMetamask  })
  H.liftEff $ UIStates.toggleLoading(".container")

checkMetamask ∷ ∀ e. Boolean → Boolean
              → H.ParentDSL State Query ChildQuery ChildSlot Void (FIDMonad e) Unit
checkMetamask loggedIn mmStatus = do
  myAddr ← H.gets _.myAddr
  if (loggedIn && mmStatus)
    then do
      newAddr ← Just <$> (H.liftAff MM.currentUserAddress)
      if isNothing myAddr || myAddr /= newAddr
        then refreshData
        else pure unit
    else refreshData

startCheckInterval maybeBus mmInterval txInterval = do
  case maybeBus of
    Nothing → pure unit
    Just bus  → do
      _ ← H.liftEff $ setInterval mmInterval $ checkMMEff  bus
      _ ← H.liftEff $ setInterval txInterval $ checkTxsEff bus
      pure unit
      where checkMMEff b = do
              _ ← launchAff $ Bus.write CheckMetamask b
              pure unit
            checkTxsEff b = do
              _ ← launchAff $ Bus.write CheckTxs b
              pure unit

runTests = do
  (H.liftAff $ F.runMonadF $ F.nameInUse "timtime")       >>= hLog
--  (H.liftAff $ F.runMonadF $ F.foundationId)       >>= hLog
--  (H.liftAff $ F.runMonadF $ F.confirmedFriends)   >>= hLog
--  (H.liftAff $ F.runMonadF $ F.pendingFriends)     >>= hLog
--  (H.liftAff $ F.runMonadF $ F.pendingDebts)       >>= hLog
--  (H.liftAff $ F.runMonadF $ F.debtBalances)       >>= hLog
  pure unit

mkFriends = do
  _ ← H.liftAff $ F.runMonadF $ F.createFriendship (F.FoundationId "timtime")
  _ ← H.liftAff $ F.runMonadF $ F.confirmFriendship (F.FoundationId "timgalebach")
  pure unit

topBar ∷ ∀ p. State → H.HTML p Query
topBar state =
  let numTxs     = A.length state.txs
      processing = numTxs /= 0
      itemStr    = if numTxs == 1 then "item" else "items"
  in
    HH.div [ HP.class_ (HH.ClassName "row top-bar")]
    [
      HH.div [ HP.class_ (HH.ClassName "col logo-section")]
      [
        HH.img [HP.src "fid@1x.png"]
        , HH.text "Friend in Debt"
      ]
      , HH.div [HP.class_ (HH.ClassName "col go-back-section")]
        [
          HH.a [HP.href "#", HP.class_ (HH.ClassName "close-pop-button"), HE.onClick $ HE.input_ $ PreviousScreen]
          [HH.i [ HP.class_ (HH.ClassName "fa fa-chevron-left")][], HH.text " Back"]
        ]
      , HH.div [HP.class_ (HH.ClassName $ "col-5 align-self-end current-transactions" <> if processing then " processing" else "") ]
        [
          HH.i [HP.class_ (HH.ClassName "transaction-spinner")][]
        , HH.span_ [HH.text $
                    "Writing " <> show (A.length state.txs) <> " " <> itemStr <> "..."]
        ]
      , HH.a [HP.href "#", HE.onClick $ HE.input_ $ RefreshData , HP.class_ (HH.ClassName $ "col-5 align-self-end reload-button" <> if processing then "" else " show-reload") ]
        [
          HH.i [HP.class_ (HH.ClassName "fa fa-refresh")][]
        ]
    ]

menu ∷ ∀ p. State → H.HTML p Query
menu state =
  HH.div
    [ HP.class_ (HH.ClassName "header-menu row")]
    [
        menuItem R.BalancesScreen state
      , menuItem R.FriendsScreen state
      , menuItem R.PendingScreen state
      , menuItem R.SettingsScreen state
    ]

menuItem ∷ ∀ p. R.Screen → State → H.HTML p Query
menuItem screen state =
  let nTodo    = state.numPendingTodo
      nFriends = state.numPendingFriends
      menuText =
        case screen of
          R.SettingsScreen →
            [ HH.i [HP.class_ (HH.ClassName "fa fa-user")][]
            , HH.text $ maybe "" show state.myId ]
          R.PendingScreen →
            [ HH.text $ R.getMenuNameFor screen ]
            <> (if (nTodo + nFriends) > 0
                then [HH.span_ [HH.text $ show (nTodo + nFriends)]]
                else [])
          _ →
            [HH.text $ R.getMenuNameFor screen]
  in HH.a
    [HP.href "#",
          HP.class_ (HH.ClassName $ "col-3 " <> if screen == state.currentScreen then "active" else ""),
          HE.onClick $ HE.input_ $ SetScreen screen] $ menuText


loggerOverlay onOff logText =
  if onOff
  then HH.div [ HP.id_ "loggerOverlay"] $
       map (\text → HH.p_ [HH.text text] ) logText
  else HH.div_ []

--keep checking if metamask is loaded
loadWeb3Loop delayMs numTriesLeft =
  if numTriesLeft > 0
    then do
      hLog "Retrying"
      H.liftAff $ delay (Milliseconds (toNumber delayMs))
      mmLoggedIn ← H.liftAff MM.loggedIn
      if mmLoggedIn
        then do
          refreshData
          H.modify (_ { loggedIn = true })
        else loadWeb3Loop delayMs (numTriesLeft - 1)
    else do
      b ← H.gets _.errorBus
      case b of
        Nothing  → pure unit
        Just bus → H.liftAff $ Bus.write NetworkError bus
      pure unit
