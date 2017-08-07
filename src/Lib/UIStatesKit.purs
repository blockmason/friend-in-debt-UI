module UI.UIStatesKit where

import FriendInDebt.Prelude
import Control.Monad.Eff           (Eff, kind Effect)
import Data.String as S

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

foreign import toggleLoadingImpl :: forall e. String -> Eff e Unit

toggleLoading :: forall e. String -> Eff e Unit
toggleLoading selector = toggleLoadingImpl selector
