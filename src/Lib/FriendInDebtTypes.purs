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
import Data.Format.Money           (formatDecimal)
import Data.Int                                                    as I
import Data.Number                                                 as N
import Data.String                 (localeCompare)
import Math                        (abs, pow)
import Data.Map                                                    as M
import Data.Array                                                  as A
import Data.Tuple                  (Tuple(..))
import Data.Foldable               (foldr)
import Data.DateTime.Instant       (instant, toDateTime)
import Data.Time.Duration          (Milliseconds(..))
import Data.DateTime               (DateTime(..))
import Network.Eth.Metamask                                        as MM
import Network.Eth.Foundation      (fiBlankId, fiMkId, FoundationId(..))

type StringAddr = String
type StringId   = String
type UserName   = String
type Description = String


-- error
data Error =
    NoMetamask
  | InvalidDebtId
  | NoFoundationId
  | TxError

instance showError ∷ Show Error where
  show NoMetamask     = "FriendInDebtError: Metamask not logged in."
  show InvalidDebtId  = "FriendInDebtError: InvalidDebtId"
  show NoFoundationId = "FriendInDebtError: NoFoundationId"
  show TxError        = "FriendInDebtError: TxError"

data DebtId =
    DebtId Number
  | NoDebtId
isValidDebtId ∷ DebtId → Boolean
isValidDebtId NoDebtId = false
isValidDebtId _        = true

getDebtId ∷ DebtId → Number
getDebtId (DebtId id) = id
getDebtId _           = -1.0

numToDate ∷ Number → Maybe DateTime
numToDate n = toDateTime <$> instant (Milliseconds n)
zeroDate ∷ Maybe DateTime
zeroDate = numToDate 0.0

-- money
data Currency =
    Currency { isoCode ∷ String, decimals ∷ Int }
  | InvalidCurrency
mkCurrency ∷ String → Int → Currency
mkCurrency isoCode decimals = Currency { isoCode: isoCode, decimals: decimals }
invalidCurrency = Currency { isoCode: "INVALID", decimals: 0 }

cIsoCode (Currency c)  = c.isoCode
cIsoCode _             = "INVALID"
cDecimals (Currency c) = c.decimals
cDecimals _            = 0

instance showCurrency ∷ Show Currency where
  show (Currency c) = c.isoCode <> ", decimals: " <> show c.decimals
  show InvalidCurrency = "InvalidCurrency"
instance eqCurrency ∷ Eq Currency where
  eq (Currency c1) (Currency c2)     = c1.isoCode == c2.isoCode
  eq InvalidCurrency InvalidCurrency = true
  eq _ _                             = false

cUSD = mkCurrency "USD" 2
cEUR = mkCurrency "EUR" 2
fromIsoCode ∷ String → Currency
fromIsoCode "USD" = cUSD
fromIsoCode "EUR" = cEUR
fromIsoCode _     = InvalidCurrency
conversionFactor ∷ Currency → Number
conversionFactor = (pow 10.0) <<< I.toNumber <<< cDecimals

newtype Money = Money { amount ∷ Number, currency ∷ Currency }

formatMoney ∷ Money → String
formatMoney m =
  let c = moneyCurrency m
  in formatDecimal ((numAmount m) / (conversionFactor c)) $ cDecimals c
instance showMoney ∷ Show Money where
  show m = cIsoCode (moneyCurrency m) <> " " <> (formatMoney $ m)
instance eqMoney ∷ Eq Money where
  eq (Money m1) (Money m2) = (m1.amount == m2.amount) && (m1.currency == m2.currency)
instance ordMoney ∷ Ord Money where
  compare (Money m1) (Money m2) = compare m1.amount m2.amount
  --TODO: make the above work with multiple currencies

moneyFromDecString ∷ String → Currency → Money
moneyFromDecString val c =
  let amount = (fromMaybe 0.0 $ N.fromString val) * (conversionFactor c)
  in mkMoney amount c
mkMoney ∷ Number → Currency → Money
mkMoney val currency = Money { amount: val, currency: currency }
numAmount ∷ Money → Number
numAmount (Money m) = m.amount
moneyCurrency ∷ Money → Currency
moneyCurrency (Money m) = m.currency

-- rewrite this function using currency converter
mockConvertCurrency :: Money → Currency → Money
mockConvertCurrency (m) (c) = m

newtype Balance = Balance { debtor     ∷ FoundationId
                          , creditor   ∷ FoundationId
                          , amount     ∷ Money
                          , totalDebts ∷ Int
                          , mostRecent ∷ Maybe DateTime }
instance showBalance ∷ Show Balance where
  show (Balance b) = (show b.debtor) <> ", " <> (show b.creditor) <> ": " <> (show b.amount)

rawToBalance ∷ FoundationId → RawBalance → Balance
rawToBalance fi rb =
  let (Tuple d c) = debtorCreditor fi rb.counterParty rb.amount
  in Balance { amount: mkMoney (abs rb.amount) (fromIsoCode rb.currency)
             , debtor: d, creditor: c
             , totalDebts: (fromMaybe 0 $ I.fromNumber rb.totalDebts)
             , mostRecent: toDateTime <$> instant (Milliseconds rb.mostRecent) }
  where debtorCreditor myId cpId val = if val >= (I.toNumber 0)
                                     then Tuple myId (FoundationId cpId)
                                     else Tuple (FoundationId cpId) myId

sumMoney ∷ ∀ e. Money → Money → Aff e Money
sumMoney m1 m2 = pure $ mkMoney ((numAmount m1) + (numAmount m2)) (moneyCurrency m1)

{- Debt -}
newtype Debt = Debt { debtor     ∷ FoundationId
                    , creditor   ∷ FoundationId
                    , toConfirm  ∷ FoundationId
                    , debt       ∷ Money
                    , debtId     ∷ DebtId
                    , desc       ∷ String
                    , timestamp  ∷ Maybe DateTime }
instance showDebt ∷ Show Debt where
  show (Debt fd) = show fd.debt <> ": " <> show fd.debtor <> " " <> show fd.creditor
    <> " | " <> show fd.desc
instance eqDebt ∷ Eq Debt where
  eq (Debt fd1) (Debt fd2) =
    (fd1.debtor == fd2.debtor) && (fd1.creditor == fd2.creditor) && (fd1.debt == fd2.debt)

rawToDebt ∷ RawDebt → Debt
rawToDebt rd =
  Debt { debtId: DebtId rd.id
       , debtor: FoundationId rd.debtor
       , creditor: FoundationId rd.creditor
       , toConfirm: FoundationId rd.confirmerId
       , debt: mkMoney rd.amount (fromIsoCode rd.currency)
       , desc: rd.desc
       , timestamp: toDateTime <$> instant (Milliseconds rd.timestamp)}

rawConfirmedToDebt ∷ RawConfirmed → Debt
rawConfirmedToDebt rd =
  Debt { debtId: NoDebtId
       , debtor: fiMkId rd.debtor
       , creditor: fiMkId rd.creditor
       , toConfirm: fiBlankId
       , debt: mkMoney rd.amount (fromIsoCode rd.currency)
       , desc: rd.desc
       , timestamp: toDateTime <$> instant (Milliseconds rd.timestamp) }

mkDebt ∷ FoundationId → FoundationId → FoundationId → Money → DebtId → Description
       → Debt
mkDebt d c toC amount dId desc = Debt { debtor: d, creditor: c, debt: amount
                                  , debtId: dId, desc: desc, toConfirm: toC
                                  , timestamp: zeroDate }
zeroDebt ∷ Currency → FoundationId → FoundationId → FoundationId → Debt
zeroDebt cur debtor creditor toConfirm = mkDebt debtor creditor toConfirm (mkMoney 0.0 cur) NoDebtId ""
debtMoney ∷ Debt → Money
debtMoney (Debt d) = d.debt
debtAmount = numAmount <<< debtMoney
debtSetDebtor   (Debt d) debtor   = Debt $ d { debtor   = debtor }
debtSetCreditor (Debt d) creditor = Debt $ d { creditor = creditor }
debtDebtor (Debt d ) = d.debtor
debtCreditor (Debt d ) = d.creditor
debtGetId (Debt d) = d.debtId
setDebt ∷ Debt → Number → Currency → Debt
setDebt (Debt fd) val currency = Debt $ fd { debt = mkMoney val currency }
setDebtMoney ∷ Debt → Money → Debt
setDebtMoney (Debt d) m = Debt $ d { debt = m }
getDesc :: Debt -> String
getDesc (Debt d) = d.desc
setDesc (Debt d) desc = Debt $ d { desc = desc }
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
               , creditor    ∷ StringId
               , timestamp   ∷ Number }

type RawConfirmed = { currency    ∷ String
                    , amount      ∷ Number
                    , desc        ∷ String
                    , debtor      ∷ StringId
                    , creditor    ∷ StringId
                    , timestamp   ∷ Number }

type RawBalance = { counterParty ∷ StringId
                  , amount       ∷ Number
                  , currency     ∷ String
                  , totalDebts   ∷ Number
                  , mostRecent   ∷ Number }

type RawFriendship = { friendId     ∷ StringId
                     , confirmerId  ∷ StringId }

{- Return Types -}
newtype PendingFriendships = PF {todo ∷ Array FoundationId, sent ∷ Array FoundationId}
instance showPendingFriendships ∷ Show PendingFriendships where
  show (PF pf) = "{todo: " <> show pf.todo <> ", sent: " <> show pf.sent <> "}"
pfGetTodos ∷ PendingFriendships → Array FoundationId
pfGetTodos (PF pf) = pf.todo
pfGetSents ∷ PendingFriendships → Array FoundationId
pfGetSents (PF pf) = pf.sent
blankPendingFriends = PF { sent: [], todo: [] }

newtype PendingDebts = PD { sent ∷ Array Debt, todo ∷ Array Debt }
instance showPendingDebts ∷ Show PendingDebts where
  show (PD pd) = "{todo: " <> show pd.todo <> ", sent: " <> show pd.sent <> "}"
pdGetTodos ∷ PendingDebts → Array Debt
pdGetTodos (PD pd) = pd.todo
pdGetSents ∷ PendingDebts → Array Debt
pdGetSents (PD pd) = pd.sent
blankPendingDebts = PD { sent: [], todo: [] }
