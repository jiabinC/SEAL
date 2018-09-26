-- | Utility functions working on Utxo.

module Pos.Txp.Toil.Utxo.Util
       ( filterUtxoByAddr
       , filterUtxoByAddrs
       , getTotalCoinsInUtxo
       , utxoToStakes
       , utxoToAddressCoinPairs
       , utxoToCurrencyAddrCoinMap
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE

import           Pos.Binary.Core ()
import           Pos.Core (Address, Coin, Currency, HasGenesisData, StakesMap, unsafeAddCoin,
                           mkCoin, pattern SealCoin)
import           Pos.Core.Txp (TxOut (..), TxOutAux (..))
import           Pos.Txp.Base (addrBelongsTo, addrBelongsToSet, txOutStake)
import           Pos.Txp.Toil.Types (Utxo, AddrCoinMap, filterUtxoPlainOnly)

-- | Select only TxOuts for given address
filterUtxoByAddr :: Address -> Utxo -> Utxo
filterUtxoByAddr addr = M.filter (`addrBelongsTo` addr)

-- | Select only TxOuts for given addresses
filterUtxoByAddrs :: [Address] -> Utxo -> Utxo
filterUtxoByAddrs addrs =
    let addrSet = HS.fromList addrs
    in  M.filter (`addrBelongsToSet` addrSet)

-- | Get total amount of coins in given Utxo
getTotalCoinsInUtxo :: Utxo -> NonEmpty (Currency, Coin)
getTotalCoinsInUtxo utxo =
    -- unsafeIntegerToCoin . sumCoins .
    -- map (txOutValue . toaOut) . toList
    NE.fromList $
    HM.toList $
    foldl' (flip $ uncurry $ HM.insertWith unsafeAddCoin) (HM.singleton SealCoin (mkCoin 0)) $
    map (\TxOutAux{..} -> (txOutCurrency toaOut, txOutValue toaOut)) $ 
    M.elems $
    filterUtxoPlainOnly utxo


-- | Convert 'Utxo' to 'StakesMap'.
utxoToStakes :: HasGenesisData => Utxo -> StakesMap
utxoToStakes = foldl' putDistr mempty . M.toList
  where
    plusAt hm (key, val) = HM.insertWith unsafeAddCoin key val hm
    putDistr hm (_, TxOutAux txOut) = foldl' plusAt hm (txOutStake txOut)

utxoToAddressCoinPairs :: Currency -> Utxo -> [(Address, Coin)]
utxoToAddressCoinPairs currency utxo = combineWith unsafeAddCoin txOuts
  where
    combineWith :: (Eq a, Hashable a) => (b -> b -> b) -> [(a, b)] -> [(a, b)]
    combineWith func = HM.toList . HM.fromListWith func

    txOuts :: [(Address, Coin)]
    txOuts = [ (addr, coin) | TxOutAux (TxOut addr c coin) <- M.elems utxo, c == currency]

utxoToCurrencyAddrCoinMap :: Utxo -> HashMap Currency AddrCoinMap
utxoToCurrencyAddrCoinMap utxo = foldl' groupByCurrency HM.empty txOuts
  where
    groupByCurrency :: HashMap Currency AddrCoinMap
                     -> (Currency, (Address, Coin))
                     -> HashMap Currency AddrCoinMap
    groupByCurrency destMap (currency, (addr, amount)) = 
            HM.insertWith (HM.unionWith unsafeAddCoin) currency (HM.singleton addr amount) destMap  

    txOuts :: [(Currency, (Address, Coin))]
    txOuts = map processTxOutAux utxoElems

    utxoElems :: [TxOutAux]
    utxoElems = M.elems $ filterUtxoPlainOnly utxo -- reserve plain TxOut only

    processTxOutAux :: TxOutAux -> (Currency, (Address, Coin))
    processTxOutAux TxOutAux {..} =  (txOutCurrency toaOut, (txOutAddress toaOut, txOutValue toaOut))