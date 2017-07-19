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
type UserName   = String
type Description = String

-- error
data Error = NoMetamask
instance showError ∷ Show Error where
  show NoMetamask = "NoMetamask: Metamask not logged in."

-- money
data Currency =
    USD
  | EUR
  | Invalid

fromString ∷ String → Currency
fromString "USD" = USD
fromString "EUR" = EUR
fromString _     = Invalid

instance showCurrency ∷ Show Currency where
  show USD = "USD"
  show EUR = "EUR"
  show _   = "Invalid"
instance eqCurrency ∷ Eq Currency where
  eq c1 c2 = (show c1) == (show c2)

newtype Money = Money { amount ∷ Number, currency ∷ Currency }

instance showMoney ∷ Show Money where
  show (Money m) =
    (if (m.amount < (toNumber 0)) then "-" else "") <> (formatDollar $ abs m.amount)
instance eqMoney ∷ Eq Money where
  eq (Money m1) (Money m2) = (m1.amount == m2.amount) && (m1.currency == m2.currency)
instance ordMoney ∷ Ord Money where
  compare (Money m1) (Money m2) = compare m1.amount m2.amount
  --TODO: make the above work with multiple currencies

mkMoney ∷ Number → String → Money
mkMoney val currencyCode = Money { amount: val, currency: fromString currencyCode }
numAmount ∷ Money → Number
numAmount (Money m) = m.amount
strCurrency ∷ Money → String
strCurrency (Money m) = show m.currency

newtype Balance = Balance { debtor     ∷ FoundationId
                          , creditor   ∷ FoundationId
                          , amount     ∷ Money }
instance showBalance ∷ Show Balance where
  show (Balance b) = (show b.debtor) <> ", " <> (show b.creditor) <> ": " <> (show b.amount)

rawToBalance ∷ FoundationId → RawBalance → Balance
rawToBalance fi rb =
  let (Tuple d c) = debtorCreditor fi rb.counterParty rb.amount
  in Balance { amount: mkMoney (abs rb.amount) rb.currency
             , debtor: d, creditor: c }
  where debtorCreditor fi cpId val = if val >= (toNumber 0)
                                     then Tuple fi (FoundationId cpId)
                                     else Tuple (FoundationId cpId) fi

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
blankFriendDebt = FriendDebt { friend: EthAddress "0x0", debt: mkMoney 0.0 "USD"
                             , debtId: 0, desc: "", currency: "USDcents" }

friendDebtZero ∷ EthAddress → FriendDebt
friendDebtZero ua = changeDebtor ua blankFriendDebt
getDebt ∷ FriendDebt → Money
getDebt (FriendDebt fd) = fd.debt
setDebt ∷ FriendDebt → Number → String → FriendDebt
setDebt (FriendDebt fd) val currency = FriendDebt $ fd { debt = mkMoney val currency}
getFriendAddr ∷ FriendDebt → EthAddress
getFriendAddr (FriendDebt fd) = fd.friend
changeDebtor ∷ EthAddress → FriendDebt → FriendDebt
changeDebtor newDebtor (FriendDebt fd) = FriendDebt $ fd {friend = newDebtor}
addDebt :: FriendDebt -> Money -> FriendDebt
addDebt fd money = setDebt fd ((numAmount (getDebt fd)) + (numAmount money)) $ strCurrency money

data DebtCompare = Positive | Negative | Zero
posNegZero ∷ FriendDebt → DebtCompare
posNegZero (FriendDebt fd) | numAmount fd.debt > 0.0 = Positive
                           | numAmount fd.debt < 0.0 = Negative
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


--raw JS Object types
type RawDebt = { friend     ∷ StringId
               , amount     ∷ Int
               , debtId     ∷ Int
               , desc       ∷ String
               , currency   ∷ String }

type RawBalance = { counterParty ∷ StringId
                  , amount       ∷ Number
                  , currency     ∷ String }
