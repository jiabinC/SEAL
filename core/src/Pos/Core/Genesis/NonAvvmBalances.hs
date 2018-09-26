{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Core.Genesis.NonAvvmBalances
       ( GenesisNonAvvmBalances (..)
       , convertNonAvvmDataToBalances
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, build, (%))
import           Serokell.Util (listJson)

import           Pos.Core.Common (Address, Currency, Coin, decodeTextAddress, unsafeAddCoin,
                                  unsafeIntegerToCoin, unsafeIntegerToCurrency)

-- | Predefined balances of non avvm entries.
newtype GenesisNonAvvmBalances = GenesisNonAvvmBalances
    { getGenesisNonAvvmBalances :: [(Address, Currency, Coin)]
    } deriving (Show, Eq)

-- instance (Hashable (Address, Currency)) =>

instance Buildable (Address, Currency, Coin) where
    build (addr, currency, coin) = bprint (build%build%build) addr currency coin

instance Buildable GenesisNonAvvmBalances where
    build (GenesisNonAvvmBalances m) =
        bprint ("GenesisNonAvvmBalances: " %listJson) m

-- deriving instance Hashable Address => Monoid GenesisNonAvvmBalances
deriving instance Monoid GenesisNonAvvmBalances

-- | Generate genesis address distribution out of avvm
-- parameters. Txdistr of the utxo is all empty. Redelegate it in
-- calling funciton.
convertNonAvvmDataToBalances
    :: forall m .
       ( MonadError Text m )
    => [(Text, Integer, Integer)]
    -> m GenesisNonAvvmBalances
convertNonAvvmDataToBalances balances = GenesisNonAvvmBalances <$> flatBalances <$> balances'
  where
    flatBalances :: HashMap (Address, Currency) Coin -> [(Address, Currency, Coin)]
    flatBalances = map (\((addr, currency), coin) -> (addr, currency, coin)) . HM.toList
    balances' :: m (HashMap (Address, Currency) Coin)
    balances' = HM.fromListWith unsafeAddCoin <$> traverse convert balances
    convert :: (Text, Integer, Integer) -> m ((Address, Currency), Coin)
    convert (txt, i1, i2) = do
        addr <- either throwError pure $ decodeTextAddress txt
        return ((addr, unsafeIntegerToCurrency i1), unsafeIntegerToCoin i2)
