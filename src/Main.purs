module Main where

import FriendInDebt.Prelude
import Halogen.Aff                                 as HA
import Halogen.VDom.Driver         (runUI)
import Container as Container
import Network.Eth.Metamask                        as MM
import Control.Monad.Eff.Class     (liftEff)
import Control.Monad.Aff.Class     (liftAff)
import Control.Monad.Aff           (launchAff)
import Control.Monad.Eff.Console   (CONSOLE, log, logShow)

main = HA.runHalogenAff $ do
  body ← HA.awaitBody
  runUI Container.ui unit body
