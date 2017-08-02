module FriendInDebt.Blockchain where

import FriendInDebt.Prelude
import FriendInDebt.Types              (ContainerMsg(..))
import Data.DateTime                   (DateTime(..))
import Data.Formatter.DateTime  as DTF
import Data.Array               as A
import Control.Monad.Aff.Bus    as Bus
import Halogen                  as H
import Network.Eth              as E
import Network.Eth.FriendInDebt as F

--helper to query the blockchain
--blankVal is a value to return if there's an error
--writes a message to the error bus if there's an error
handleCall errorBus blankVal affCall = do
  case errorBus of
    Nothing → do
      hLog "No bus initialized"
      pure blankVal
    Just b → do
      result ← H.liftAff $ F.runMonadF affCall
      case result of
        Left error → do _ ← H.liftAff $ Bus.write (FIDError error) b
                        pure blankVal
        Right val  → pure val

setWatchTx message tx = if E.isBlank tx then pure unit else H.raise $ message tx

handleTx message state fnToRun = do
  tx ← handleCall state.errorBus E.blankTx fnToRun
  setWatchTx message tx
--  H.raise $ ScreenChange

hasNetworkError ∷ Array E.TxStatus → Boolean
hasNetworkError = not ∘ A.null ∘ (A.filter E.hasError)

formatDate ∷ DateTime → String
formatDate = (either (const "") id) ∘ (DTF.formatDateTime "YYYY-MM-DD")
