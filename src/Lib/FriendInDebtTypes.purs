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

data DebtId =
    DebtId Int
  | NoDebtId

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

fetch ∷ ∀ e. Aff e Json

bind ∷ ∀ m a b. Monad m => m a → (a → m b) → m b

sumMoney ∷ ∀ e. Money → Money → Aff e Money
sumMoney m1 m2 = pure $ mkMoney ((numAmount m1) + (numAmount m2)) (strCurrency m1)

{- Debt -}
newtype Debt = Debt { debtor     ∷ FoundationId
                    , creditor   ∷ FoundationId
                    , toConfirm  ∷ FoundationId
                    , debt       ∷ Money
                    , debtId     ∷ DebtId
                    , desc       ∷ String }
instance showDebt ∷ Show Debt where
  show (Debt fd) = show fd.debt <> ": " <> show fd.debtor <> " " <> show fd.creditor
instance eqDebt ∷ Eq Debt where
  eq (Debt fd1) (Debt fd2) =
    (fd1.debtor == fd2.debtor) && (fd1.creditor == fd2.creditor) && (fd1.debt == fd2.debt)

rawToDebt ∷ RawDebt → Debt
rawToDebt rd = Debt { debtId: DebtId rd.id
                    , debtor: FoundationId rd.debtor
                    , creditor: FoundationId rd.creditor
                    , toConfirm: rd.confirmerId
                    , debt: mkMoney rd.amount $ fromString rd.currency
                    , desc: desc }

mkDebt ∷ FoundationId → FoundationId → FoundationId → Money → DebtId → Description
       → Debt
mkDebt d c toC amount dId desc = Debt { debtor: d, creditor: c, debt: amount
                                  , debtId: dId, desc: desc, toConfirm: toC}
zeroDebt ∷ Currency → FoundationId → FoundationId → FoundationId → Debt
zeroDebt cur debtor creditor toConfirm = mkDebt debtor creditor toConfirm (mkMoney 0.0 cur) NoDebtId ""
fdDebt ∷ Debt → Money
fdDebt (Debt fd) = fd.debt
setDebt ∷ Debt → Number → String → Debt
setDebt (Debt fd) val currency = Debt $ fd { debt = mkMoney val currency}
debtToConfirm ∷ Debt → FoundationId
debtToConfirm (Debt d) = d.toConfirm
debtCounterparty ∷ FoundationId → Debt → FoundationId
debtCounterparty myId (Debt fd) = if fd.debtor == myId
                                 then fd.creditor else fd.debtor

{- ETH Address -}
newtype EthAddress = EthAddress StringAddr
instance showEthAddress ∷ Show EthAddress where
  show (EthAddress ua) = ua
instance eqEthAddress ∷ Eq EthAddress where
  eq (EthAddress ua1) (EthAddress ua2) = ua1 == ua2
instance ordEthAddress ∷ Ord EthAddress where
  compare (EthAddress ua1) (EthAddress ua2) = localeCompare ua1 ua2
getAddr ∷ EthAddress → String
getAddr (EthAddress ea) = ea

--raw JS Object types
type RawDebt = { id          ∷ Number
               , confirmerId ∷ StringId
               , currency    ∷ String
               , amount      ∷ Number
               , desc        ∷ String
               , debtor      ∷ StringId
               , creditor    ∷ StringId }

type RawBalance = { counterParty ∷ StringId
                  , amount       ∷ Number
                  , currency     ∷ String }
