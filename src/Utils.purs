module Utils where

import FriendInDebt.Prelude
import Control.Monad.Eff.Console (logShow)
import Halogen as H

hLog = H.liftEff ∘ logShow
