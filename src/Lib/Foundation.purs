module Network.Eth.Foundation
       (
         FoundationId(..)
       , fiGetId
       , fiBlankId
       , fiMkId
       , fiStrValidId
       , initial
       ) where

import Prelude
import Data.String                 as S
import Data.String.Regex.Unsafe    as RU
import Data.String.Regex           as R
import Data.String.Regex.Flags     as RF

newtype FoundationId = FoundationId String
instance showFoundationId ∷ Show FoundationId where
  show (FoundationId fi) = fi
instance eqFoundationId ∷ Eq FoundationId where
  eq (FoundationId fi1) (FoundationId fi2) = fi1 == fi2
instance ordFoundationId ∷ Ord FoundationId where
  compare (FoundationId fi1) (FoundationId fi2) = S.localeCompare fi1 fi2
fiMkId str = (FoundationId $ S.toLower str)
fiGetId (FoundationId fi) = fi
fiBlankId = (FoundationId "")
initial ∷ FoundationId → String
initial fid = S.take 1 $ S.toLower $ show fid

fiStrValidId ∷ String → Boolean
fiStrValidId s = S.length s > 3 && S.length s < 33 && R.test idRegex s
idRegex ∷ R.Regex
idRegex = RU.unsafeRegex "^([a-z]|\\d|-|\\.)+$" RF.noFlags
