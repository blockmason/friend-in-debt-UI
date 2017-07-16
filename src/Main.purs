module Main where

import FriendInDebt.Prelude

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Container as Container

main = HA.runHalogenAff $ do
  body ← HA.awaitBody
  runUI Container.ui unit body
