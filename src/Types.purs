module Types where

import FriendInDebt.Prelude
import Data.String (joinWith)
import Data.Format.Money (formatDollar)
import Data.Int (toNumber)
import Data.Map    as M
import Network.HTTP.Affjax (AJAX)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Random (RANDOM, random)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Eff.Exception (EXCEPTION)
import Network.Eth.Metamask (MetamaskStatus(..), METAMASK)
import Network.Eth.FriendInDebt as F

type NameMap = M.Map F.EthAddress String
type DebtMap = M.Map F.EthAddress F.FriendDebt

------------------- App Monad(s) ---------------------------
type FIDMonad eff = (Aff (exception ∷ EXCEPTION, timer ∷ TIMER, random ∷ RANDOM, avar ∷ AVAR, console ∷ CONSOLE, ajax ∷ AJAX, fid ∷ F.FID, metamask ∷ METAMASK | eff))

------------------- App State -----------------------------
data ContainerMsg
  = FIDError F.Error
  | CheckMetamask
instance showContainerMsg ∷ Show ContainerMsg where
  show (FIDError fe) = show fe
  show CheckMetamask = "Checking Metamask status."

type ContainerMsgBus = Maybe (Bus.BusRW ContainerMsg)
