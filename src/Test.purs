module Test.Main where

import FriendInDebt.Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff.Class     (liftAff)
import Control.Monad.Aff (launchAff, Aff(..))
import FriendInDebt.Types as T
import Network.HTTP.Affjax (affjax, defaultRequest, AJAX)
import Network.Eth.FriendInDebt as F

runTests = launchAff $ do
  (F.runMonadF F.foundationId) >>= (liftEff <<< logShow)
