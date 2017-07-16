module Network.Eth.FoundationId
       (
         FoundationId(..)
       ) where

import Prelude
import Data.String                 (localeCompare)

newtype FoundationId = FoundationId String
instance showFoundationId ∷ Show FoundationId where
  show (FoundationId fi) = fi
instance eqFoundationId ∷ Eq FoundationId where
  eq (FoundationId fi1) (FoundationId fi2) = fi1 == fi2
instance ordFoundationId ∷ Ord FoundationId where
  compare (FoundationId fi1) (FoundationId fi2) = localeCompare fi1 fi2
