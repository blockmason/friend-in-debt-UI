module Data.Format.Money
       (
         formatDollar
       , formatDecimal
       , MoneyAmount
       , Decimals
       ) where

type Decimals = Int
type MoneyAmount = Number

foreign import formatDollar  :: MoneyAmount → String
foreign import formatDecimal :: MoneyAmount → Decimals → String
