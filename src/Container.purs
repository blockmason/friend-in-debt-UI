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

data Query a
  = Init a
  | HandleMsg ContainerMsg a
  | RefreshMetamask a
  | SetScreen R.Screen a
  | PreviousScreen a
  | DebtViewMsg D.Message a

type State = { loggedIn ∷ Boolean
             , loading  ∷ Boolean
             , errorBus ∷ ContainerMsgBus
             , txs      ∷ Array E.TX
             , currentScreen ∷ R.Screen
             , history ∷ Array R.Screen }

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
    initialState = { loggedIn: true
                   , loading: true
                   , errorBus: Nothing
                   , txs: []
                   , currentScreen: R.BalancesScreen
                   , history: []}

    render ∷ State → H.ParentHTML Query ChildQuery ChildSlot (FIDMonad eff)
    render state =
      HH.div [ HP.id_ "container", HP.class_ (HH.ClassName $ "container " <> (R.getRouteNameFor state.currentScreen)  <> (if state.loading then " loading" else "")) ]
      [ promptMetamask state.loggedIn
      , loadingOverlay state.loading
      , HH.div [ HP.id_ "back-nav-bar", HP.class_ (HH.ClassName "row back-nav-bar")]
        [
          HH.a [HP.href "#", HP.class_ (HH.ClassName "close-pop-button"), HE.onClick $ HE.input_ $ PreviousScreen]
          [HH.i [ HP.class_ (HH.ClassName "fa fa-chevron-left")][], HH.text " Back"]
        ]
      -- , HH.div [ HP.id_ "home-bar", HP.class_ (HH.ClassName "row home-bar")]
      --   [
      --     HH.a [HP.href "#", HP.class_ (HH.ClassName "col home"), HE.onClick $ HE.input_ $ SetScreen "show-balances"] [
      --           HH.img [HP.src "http://blockmason.io/assets/img/friends_in_debt_logo.svg"], HH.text "Friend in Debt"]
      --   ]
      -- , HH.div [ HP.id_ "header", HP.class_ (HH.ClassName "row")]
      --   [
      --     HH.a [HP.href "#", HP.class_ (HH.ClassName $ "col-3 " <> if state.currentScreen == "show-balances" then "active" else ""), HE.onClick $ HE.input_ $ SetScreen "show-balances"] [ HH.text "Balances"],
      --     HH.a [HP.href "#", HP.class_ (HH.ClassName $ "col-3 " <> if state.currentScreen == "show-friends" then "active" else "" ), HE.onClick $ HE.input_ $ SetScreen "show-friends"] [ HH.text "Friends"],
      --     HH.a [HP.href "#", HP.class_ (HH.ClassName $ "col-3 " <> if state.currentScreen == "show-pending" then "active" else ""), HE.onClick $ HE.input_ $ SetScreen "show-pending"] [ HH.text "Pending"],
      --     HH.a [HP.href "#", HP.class_ (HH.ClassName $ "col-3 " <> if state.currentScreen == "show-settings" then "active" else ""), HE.onClick $ HE.input_ $ SetScreen "show-settings"] [ HH.text "Settings"]
      --   ]
      -- , HH.div [ HP.class_ (HH.ClassName "row create-debt-bar") ]
      -- [
      --   HH.a [HP.href "#", HP.class_ (HH.ClassName ""), HE.onClick $ HE.input_ $ SetScreen "show-create-debt"] [
      --   HH.i [ HP.class_ (HH.ClassName "fa fa-plus-circle")][], HH.text " Create Debt"]
      -- ]
      -- , HH.div [ HP.class_ (HH.ClassName "row add-friend-bar") ]
      -- [
      --   HH.a [HP.href "#", HP.class_ (HH.ClassName ""), HE.onClick $ HE.input_ $ SetScreen "show-add-friend"] [
      --   HH.i [ HP.class_ (HH.ClassName "fa fa-plus-circle")][], HH.text " Add Friend"]
      -- ]
      , HH.div [ HP.class_ (HH.ClassName "row")]
        [
          HH.slot' CP.cp1 unit D.component state.errorBus $ HE.input DebtViewMsg
        ]
      ]

    eval ∷ Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (FIDMonad eff)
    eval = case _ of
      Init next → do
        bus ← H.liftAff $ Bus.make
        H.subscribe $ busEventSource (flip HandleMsg ES.Listening) bus
        H.modify (_ { loggedIn = true, loading = true, errorBus = Just bus })
        H.liftAff $ delay (Milliseconds (toNumber 1500))
        H.modify (_ { loading = false })
        refreshMetamask
        runTests
        startCheckInterval (Just bus) 5000
        pure next
      HandleMsg msg next → do
        case msg of
          FIDError e → do
            H.modify (_ { loggedIn = false })
            pure next
          CheckMetamask → do
            mmStatus ← MM.loggedIn <$> (H.liftEff MM.checkStatus)
            loggedIn ← H.gets _.loggedIn
            checkMetamask loggedIn mmStatus
            pure next
      RefreshMetamask next → do
        refreshMetamask
        pure next
      SetScreen screen next → do
        H.modify (\state → state {history = append [state.currentScreen] state.history })
        H.modify (_ {currentScreen = screen})
        pure next
      DebtViewMsg msg next →
        case msg of
          D.ScreenChange screen → do
            H.modify (\state → state {history = append [state.currentScreen] state.history })
            H.modify (_ {currentScreen = screen})
            pure next
          D.NewTX newTx → do
            H.modify (\s → s { txs = s.txs <> [newTx] })
            pure next
      PreviousScreen next → do
        H.modify (\state → state {currentScreen = (fromMaybe R.BalancesScreen $ A.head state.history), history = (fromMaybe [] $ A.tail state.history)})
        pure next

loadingOverlay ∷ ∀ p i. Boolean → H.HTML p i
loadingOverlay loading =
  HH.div [ HP.id_ "loadingOverlay"
         , if loading then HP.class_ (HH.ClassName "active")
           else HP.class_ (HH.ClassName "inActive")]
  [ HH.h6_ [ HH.text "Loading Metamask..." ]]

promptMetamask ∷ ∀ p. Boolean → H.HTML p Query
promptMetamask loggedIn =
  HH.div [ HP.id_ "metamaskOverlay"
         , if loggedIn then HP.class_ (HH.ClassName "inActive")
           else HP.class_ (HH.ClassName "active")]
  [ HH.div_
    [ HH.h6_ [ HH.text "Not logged in to Metamask." ]
    , HH.button [ HE.onClick $ HE.input_ $ RefreshMetamask
                , HP.class_ $ HH.ClassName "btn-info"]
      [ HH.text "Retry" ]]]

refreshMetamask ∷ ∀ e. H.ParentDSL State Query ChildQuery ChildSlot Void (FIDMonad e) Unit
refreshMetamask = do
  mmStatus ← MM.loggedIn <$> (H.liftEff MM.checkStatus)
  if mmStatus
    then do _ ← H.query' CP.cp1 unit (D.RefreshDebts unit)
            newmmStatus ← MM.loggedIn <$> (H.liftEff MM.checkStatus)
            H.modify (_ { loggedIn = newmmStatus })
    else do H.modify (_ { loggedIn = mmStatus })

checkMetamask ∷ ∀ e. Boolean → Boolean
              → H.ParentDSL State Query ChildQuery ChildSlot Void (FIDMonad e) Unit
checkMetamask loggedIn mmStatus =
  if (loggedIn && mmStatus) then pure unit else refreshMetamask

startCheckInterval maybeBus ms = do
  case maybeBus of
    Nothing → pure unit
    Just b  → do
      _ ← H.liftEff $ setInterval ms $ effToRun b
      pure unit
      where effToRun bus = do
              _ ← launchAff $ Bus.write CheckMetamask bus
              pure unit


runTests = do
  (H.liftAff $ F.runMonadF $ F.foundationId)       >>= hLog
  pure unit

mkFriends = do
  _ ← H.liftAff $ F.runMonadF $ F.createFriendship (F.FoundationId "timtime")
  _ ← H.liftAff $ F.runMonadF $ F.confirmFriendship (F.FoundationId "timgalebach")
  pure unit
