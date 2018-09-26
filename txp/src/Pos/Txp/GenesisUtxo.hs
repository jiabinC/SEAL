{-# LANGUAGE NamedFieldPuns #-}

-- | Runtime propagation of genesis data (stakes & utxo).

module Pos.Txp.GenesisUtxo
       ( genesisUtxo
       , genesisStakes
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map

import           Pos.Core (Address, Currency (..), Coin, GenesisData (..), StakesMap, HasGenesisData,
                           genesisData, getGenesisAvvmBalances, getGenesisNonAvvmBalances,
                           makeRedeemAddress, pattern SealCoin)
import           Pos.Core.Txp (TxIn (..), TxOut (..), TxOutAux (..))
import           Pos.Crypto (unsafeHash)
import           Pos.Txp.Toil (GenesisUtxo (..), utxoToStakes)


genesisStakes :: HasGenesisData => StakesMap
genesisStakes = utxoToStakes . unGenesisUtxo $ genesisUtxo

genesisUtxo :: HasGenesisData => GenesisUtxo
genesisUtxo =
    let GenesisData{ gdNonAvvmBalances
                   , gdAvvmDistr
                   } = genesisData

        preUtxo :: [(Address, Currency, Coin)]
        -- preUtxo = (first makeRedeemAddress <$> HM.toList (getGenesisAvvmBalances gdAvvmDistr))
        --                           <> (HM.toList $ getGenesisNonAvvmBalances gdNonAvvmBalances)
        preUtxo = avvmBalances <> nonAvvmBalances
            where
                originAvvmBalances = (first makeRedeemAddress <$> HM.toList (getGenesisAvvmBalances gdAvvmDistr))
                avvmBalances = map (\(a, b) -> (a, SealCoin, b)) originAvvmBalances
                nonAvvmBalances = getGenesisNonAvvmBalances gdNonAvvmBalances

        utxoEntry :: (Address, Currency, Coin) -> (TxIn, TxOutAux)
        utxoEntry (addr, currency, coin) =
                 ( TxInUtxo (unsafeHash addr) $ getCurrency currency
                 , TxOutAux (TxOut addr currency coin)
                 )

     in GenesisUtxo . Map.fromList $ utxoEntry <$> preUtxo
