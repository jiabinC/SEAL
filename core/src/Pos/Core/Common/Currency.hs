{-# LANGUAGE RankNTypes #-}
module Pos.Core.Common.Currency
       ( Currency (..)
       , sealCurrency
       , pattern SealCoin
       , pattern GoldCoin
       , pattern GoldDollar
       , mkCurrency
       , checkCurrency
       , currencyF

       , minCurrencyVal
       , maxCurrencyVal
       , integerToCurrency
       , currencyToInteger
       , unsafeIntegerToCurrency
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))
import           Data.Data (Data)
import qualified Data.Text.Buildable
import           Formatting (Format, bprint, build, int, (%))

import           Pos.Binary.Class (Bi (..))
import           Text.Read (read)
import           Data.Text (pack)
import           Data.Aeson (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..),
                             ToJSON (toJSON), ToJSONKey (..))
import           Data.Aeson.Types (toJSONKeyText)
import           Pos.Util.Util (leftToPanic)

newtype Currency = Currency
    { getCurrency :: Word32
    } deriving (Show, Ord, Eq, Generic, Hashable, Data, NFData)

instance Buildable Currency where
    build SealCoin = bprint "SealCoin"
    build GoldCoin = bprint "GoldCoin"
    build GoldDollar = bprint "GoldDollar"
    build (Currency n) = bprint ("currency "%int) n

instance Bounded Currency where
    minBound = Currency minCurrencyVal
    maxBound = Currency maxCurrencyVal

-- seal coin's currency
sealCurrency :: Currency
sealCurrency = Currency 0

-- | Maximal possible value of 'Currency'.
minCurrencyVal :: Word32
minCurrencyVal = 0

-- | Maximal possible value of 'Currency'.
maxCurrencyVal :: Word32
maxCurrencyVal = maxBound

-- | Makes a 'Currency' but is _|_ if that currency value does't in [0, maxCurrencyVal]'.
-- You can also use 'checkCurrency' to do that check.
mkCurrency :: Word32 -> Currency
mkCurrency c = either error (const currency) (checkCurrency currency)
  where
    currency = (Currency c)
{-# INLINE mkCurrency #-}

checkCurrency :: MonadError Text m => Currency -> m ()
checkCurrency (Currency c)
    | c >= 0 && c <= maxCurrencyVal = pure () 
    | otherwise                     = throwError $ "Currency: " <> show c <> " is invalid"

-- | Currency formatter which restricts type.
currencyF :: Format r (Currency -> r)
currencyF = build

currencyToInteger :: Currency -> Integer
currencyToInteger = toInteger . getCurrency
{-# INLINE currencyToInteger #-}

integerToCurrency :: Integer -> Either Text Currency
integerToCurrency n
    | n < 0 = Left $ "integerToCurrency: value is negative (" <> show n <> ")"
    | n <= currencyToInteger (maxBound :: Currency) = pure $ Currency (fromInteger n)
    | otherwise = Left $ "integerToCurrency: value is too big (" <> show n <> ")"

unsafeIntegerToCurrency :: Integer -> Currency
unsafeIntegerToCurrency n = leftToPanic "unsafeIntegerToCurrency: " (integerToCurrency n)

pattern SealCoin :: Currency
pattern SealCoin = Currency 0

pattern GoldCoin :: Currency
pattern GoldCoin = Currency 1

pattern GoldDollar :: Currency
pattern GoldDollar = Currency 2

instance Bi Currency where
    encode = encode . getCurrency
    decode = Currency <$> decode

instance FromJSON Currency where
    parseJSON v = mkCurrency <$> parseJSON v

instance ToJSON Currency where
    toJSON = toJSON . getCurrency

instance FromJSONKey Currency where
    fromJSONKey = FromJSONKeyText (mkCurrency . read . toString)

instance ToJSONKey Currency where
    toJSONKey = toJSONKeyText (pack . show . getCurrency)

