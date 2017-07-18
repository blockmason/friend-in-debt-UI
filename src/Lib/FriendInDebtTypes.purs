module Network.Eth.FriendInDebt.Types where

import Prelude
import Network.Eth.FriendInDebt.Types
import Control.Monad.Eff           (Eff, kind Effect)
import Math                        (abs)
import Control.Monad.Eff.Class     (liftEff)
import Control.Monad.Aff.Class     (liftAff)
import Control.Monad.Aff           (Aff, makeAff)
import Control.Monad.Except.Trans  (ExceptT, throwError, runExceptT, lift)
import Data.Either                 (Either(Left, Right))
import Data.Maybe                  (Maybe(..), fromMaybe)
import Data.Traversable            (traverse)
import Data.Format.Money           (formatDollar)
import Data.Int                    (toNumber)
import Data.String                 (localeCompare)
import Math                        (abs)
import Data.Map                                                    as M
import Data.Array                                                  as A
import Data.Tuple                  (Tuple(..))
import Data.Foldable               (foldr)
import Network.Eth.Metamask                                        as MM
import Network.Eth.Foundation      (FoundationId(..))

type StringAddr = String
type StringId   = String
type DummyVal   = String
type UserName   = String

-- error
data Error = NoMetamask
instance showError ∷ Show Error where
  show NoMetamask = "NoMetamask: Metamask not logged in."

-- money
newtype Money = Money Number
instance showMoney ∷ Show Money where
  show (Money val) =
    (if (val < (toNumber 0)) then "-" else "") <> (formatDollar $ abs val)
instance eqMoney ∷ Eq Money where
  eq (Money m1) (Money m2) = m1 == m2
instance ordMoney ∷ Ord Money where
  compare (Money m1) (Money m2) = compare m1 m2

amount ∷ Money → Number
amount (Money m) = m
absMoney ∷ Money → Money
absMoney (Money m) = Money $ abs m
mkNegative ∷ Money → Money
mkNegative (Money m) = Money $ m * (-1.0)

-- FriendDebt
newtype FriendDebt = FriendDebt { friend     ∷ EthAddress
                                , debt       ∷ Money
                                , debtId     ∷ Int
                                , desc       ∷ String
                                , currency   ∷ String }
instance showFriendDebt ∷ Show FriendDebt where
  show (FriendDebt fd) = show fd.debt <> ": " <> show fd.friend
instance eqFriendDebt ∷ Eq FriendDebt where
  eq (FriendDebt fd1) (FriendDebt fd2) =
    (fd1.friend == fd2.friend) && (fd1.debt == fd2.debt)

blankFriendDebt ∷ FriendDebt
blankFriendDebt = FriendDebt { friend: EthAddress "0x0", debt: Money 0.0
                             , debtId: 0, desc: "", currency: "USDcents" }

friendDebtZero ∷ EthAddress → FriendDebt
friendDebtZero ua = changeDebtor ua blankFriendDebt
getDebt ∷ FriendDebt → Money
getDebt (FriendDebt fd) = fd.debt
setDebt ∷ FriendDebt → Number → FriendDebt
setDebt (FriendDebt fd) m = FriendDebt $ fd { debt = (Money m)}
getFriendAddr ∷ FriendDebt → EthAddress
getFriendAddr (FriendDebt fd) = fd.friend
changeDebtor ∷ EthAddress → FriendDebt → FriendDebt
changeDebtor newDebtor (FriendDebt fd) = FriendDebt $ fd {friend = newDebtor}
newDebt ∷ EthAddress → Number → FriendDebt
newDebt f d = FriendDebt { friend: f, debt: Money d, debtId: 0, desc: "", currency: "USDcents" }
addDebt :: FriendDebt -> Money -> FriendDebt
addDebt fd money = setDebt fd $ (amount (getDebt fd)) + (amount money)
--make a debt value negative
flipDebt ∷ FriendDebt → FriendDebt
flipDebt (FriendDebt fd) = FriendDebt $ fd { debt = mkNegative fd.debt }

data DebtCompare = Positive | Negative | Zero
posNegZero ∷ FriendDebt → DebtCompare
posNegZero (FriendDebt fd) | amount fd.debt > 0.0 = Positive
                           | amount fd.debt < 0.0 = Negative
                           | otherwise            = Zero

-- ETH Address
newtype EthAddress = EthAddress StringAddr
instance showEthAddress ∷ Show EthAddress where
  show (EthAddress ua) = ua
instance eqEthAddress ∷ Eq EthAddress where
  eq (EthAddress ua1) (EthAddress ua2) = ua1 == ua2
instance ordEthAddress ∷ Ord EthAddress where
  compare (EthAddress ua1) (EthAddress ua2) = localeCompare ua1 ua2
getUa ∷ EthAddress → String
getUa (EthAddress ua) = ua
