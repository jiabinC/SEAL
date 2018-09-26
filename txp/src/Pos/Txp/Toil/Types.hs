{-# LANGUAGE TypeFamilies #-}

-- | Types used for pure transaction processing (aka Toil).

module Pos.Txp.Toil.Types
       ( Utxo
       , UtxoLookup
       , UtxoModifier
       , formatUtxo
       , utxoF
       , utxoToModifier
       , utxoToLookup
       , GenesisUtxo (..)
       , _GenesisUtxo

       , StakesView (..)
       , svStakes
       , svTotal

       , TxFee(..)
       , MemPool (..)
       , mpLocalTxs
       , mpSize
       , TxMap
       , UndoMap
       , AddrCoinMap
       , CurrencyCoinMap
       , filterUtxoByCurrency
       , filterUtxoCertOnly
       , filterUtxoPlainOnly
       , filterUtxoModifierByCurrency
       , applyUtxoModToAddrCoinMap
       , applyUtxoModToCurrencyAddrCoinMapMap
       , getBalanceFromCurrencyAddrCoinMapMap
       , getGoldCoinStateOnly
       , getGoldDollarStateOnly
       ) where

import           Universum

import           Control.Lens (makeLenses, makePrisms, makeWrapped)
import           Data.Default (Default, def)
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M (lookup, member, toList)
import           Data.Text.Lazy.Builder (Builder)
import           Formatting (Format, later)
import           Serokell.Util.Text (mapBuilderJson)

import           Pos.Core (Address, Coin, Currency, StakeholderId, unsafeAddCoin,
                 unsafeSubCoin, mkCoin, pattern SealCoin, pattern GoldCoin, pattern GoldDollar)
import           Pos.Core.Txp (TxAux, TxId, TxIn, TxOut (..), TxOutAux (..), TxUndo, 
                 matchCurrency, isPlainTxOut, isCertTxOut, isGoldCoinStateTxOut, isGoldDollarStateTxOut)
import qualified Pos.Util.Modifier as MM

----------------------------------------------------------------------------
-- UTXO
----------------------------------------------------------------------------

-- | Unspent transaction outputs.
--
-- Transaction inputs are identified by (transaction ID, index in list of
-- output) pairs.
type Utxo = Map TxIn TxOutAux

-- | Type of function to look up an entry in 'Utxo'.
type UtxoLookup = TxIn -> Maybe TxOutAux

-- | All modifications (additions and deletions) to be applied to 'Utxo'.
type UtxoModifier = MM.MapModifier TxIn TxOutAux

-- | Format 'Utxo' map for showing
formatUtxo :: Utxo -> Builder
formatUtxo = mapBuilderJson . M.toList

-- | Specialized formatter for 'Utxo'.
utxoF :: Format r (Utxo -> r)
utxoF = later formatUtxo

utxoToModifier :: Utxo -> UtxoModifier
utxoToModifier = foldl' (flip $ uncurry MM.insert) mempty . M.toList

utxoToLookup :: Utxo -> UtxoLookup
utxoToLookup = flip M.lookup

filterUtxoByCurrency :: Utxo -> Currency -> Utxo
filterUtxoByCurrency utxo currency = M.filter (matchCurrency currency . toaOut) utxo

filterUtxoPlainOnly :: Utxo -> Utxo
filterUtxoPlainOnly = M.filter (isPlainTxOut . toaOut)

filterUtxoCertOnly :: Utxo -> Utxo
filterUtxoCertOnly = M.filter (isCertTxOut . toaOut)

filterUtxoModifierByCurrency :: UtxoModifier -> Currency -> UtxoModifier
filterUtxoModifierByCurrency utxoModifier currency = MM.filter (maybe True (matchCurrency currency) . fmap toaOut) utxoModifier  -- reserve all deletions

getGoldCoinStateOnly :: Utxo -> Either Text (TxIn, TxOutAux)
getGoldCoinStateOnly utxo = 
    case (M.toList $ M.filter (isGoldCoinStateTxOut . toaOut) utxo) of
        [(txIn, txOutAux)] -> Right (txIn, txOutAux)
        _                  -> Left "Gold coin state not found or found more than one!"

getGoldDollarStateOnly :: Utxo -> Either Text (TxIn, TxOutAux)
getGoldDollarStateOnly utxo = 
    case (M.toList $ M.filter (isGoldDollarStateTxOut . toaOut) utxo) of
        [(txIn, txOutAux)] -> Right (txIn, txOutAux)
        _                  -> Left "Gold dollar state not found or found more than one!"

-- | Wrapper for genesis utxo.
newtype GenesisUtxo = GenesisUtxo
    { unGenesisUtxo :: Utxo
    } deriving (Show)

makePrisms  ''GenesisUtxo
makeWrapped ''GenesisUtxo

----------------------------------------------------------------------------
-- Fee
----------------------------------------------------------------------------

-- | tx.fee = sum(tx.in) - sum (tx.out)
newtype TxFee = TxFee Coin
    deriving (Show, Eq, Ord, Generic, Buildable)

----------------------------------------------------------------------------
-- StakesView
----------------------------------------------------------------------------

data StakesView = StakesView
    { _svStakes :: !(HashMap StakeholderId Coin)
    , _svTotal  :: !(Maybe Coin)
    }

makeLenses ''StakesView

instance Default StakesView where
    def = StakesView mempty Nothing

----------------------------------------------------------------------------
-- MemPool
----------------------------------------------------------------------------

type TxMap = HashMap TxId TxAux

data MemPool = MemPool
    { _mpLocalTxs :: !TxMap
      -- | Number of transactions in the memory pool.
    , _mpSize     :: !Int
    }

makeLenses ''MemPool

instance Default MemPool where
    def =
        MemPool
        { _mpLocalTxs = mempty
        , _mpSize     = 0
        }

----------------------------------------------------------------------------
-- UndoMap and AddrCoinsMap
----------------------------------------------------------------------------

type UndoMap = HashMap TxId TxUndo
type AddrCoinMap = HashMap Address Coin
type CurrencyCoinMap = HashMap Currency Coin

-- | Takes utxo modifier and address-coin map with correspodning utxo
-- and applies utxo modifier to map.
-- Works for O(size of modifier * log (size of map)).
applyUtxoModToAddrCoinMap
    :: UtxoModifier
    -> (AddrCoinMap, Utxo)
    -> AddrCoinMap
applyUtxoModToAddrCoinMap modifier (addrCoins, utxo) = result
  where
    outToPair :: TxOutAux -> (Address, Coin)
    outToPair TxOutAux {..} =  (txOutAddress toaOut, txOutValue toaOut)

    -- Resolve TxOut for every TxIn and convert TxOuts
    -- to pairs (Address, Coin)
    resolvedAddrs :: [(Address, Coin)]
    resolvedAddrs =
        mapMaybe (fmap outToPair . flip M.lookup utxo)
                 (MM.deletions modifier)

    -- subAddress and updateHM are used to do
    -- hashMap[address] = hashMap[address] - coins
    subAddress :: Coin -> Coin -> Maybe Coin
    subAddress r c = if r < c then Just (c `unsafeSubCoin` r) else Nothing

    updateHM :: HashMap Address Coin -> (Address, Coin) -> HashMap Address Coin
    updateHM hm (ad, coins) = HM.update (subAddress coins) ad hm

    -- Substract coins from current balances
    addrCoinsRest :: HashMap Address Coin
    addrCoinsRest = foldl' updateHM addrCoins resolvedAddrs

    -- Remove such TxIns which are already in wallet utxo.
    insertionsNotInUtxo :: [(TxIn, TxOutAux)]
    insertionsNotInUtxo = filter (not . flip M.member utxo . fst) (MM.insertions modifier)

    -- Convert TxOuts of insertionsNotInUtxo to [(Address, Coin)]
    addrCoinsAdditions :: [(Address, Coin)]
    addrCoinsAdditions = map (outToPair . snd) insertionsNotInUtxo

    -- Add coins to balances
    result :: HashMap Address Coin
    result = foldl' (flip $ uncurry $ HM.insertWith unsafeAddCoin) addrCoinsRest addrCoinsAdditions

applyUtxoModToCurrencyAddrCoinMapMap 
    :: UtxoModifier 
    -> Utxo 
    -> HashMap Currency AddrCoinMap
    -> HashMap Currency AddrCoinMap
applyUtxoModToCurrencyAddrCoinMapMap modifier utxo currencyAddrCoinMapMap =
        HM.mapWithKey filterAndApply preparedMM
    where
        prepareCurrency :: Currency -> HashMap Currency AddrCoinMap -> HashMap Currency AddrCoinMap 
        prepareCurrency c mm 
            | not $ c `HM.member` mm = HM.insert c HM.empty mm
            | otherwise = mm

        filterAndApply :: Currency -> AddrCoinMap -> AddrCoinMap
        filterAndApply currency addrCoins = 
            let filteredUtxo = filterUtxoByCurrency utxo currency in
            let filteredModifier = filterUtxoModifierByCurrency modifier currency in
            applyUtxoModToAddrCoinMap filteredModifier (addrCoins, filteredUtxo)

        preparedMM = prepareCurrency GoldCoin $ 
                     prepareCurrency GoldDollar $
                     prepareCurrency SealCoin currencyAddrCoinMapMap
        
-- | if address does not exists, the returns map at least contains a seal currency and 0 amount entry 
getBalanceFromCurrencyAddrCoinMapMap 
    :: Address 
    -> HashMap Currency AddrCoinMap
    -> CurrencyCoinMap 
getBalanceFromCurrencyAddrCoinMapMap addr currencyAddrCoinMapMap =
        HM.foldlWithKey' gatherBalanceByCurrency (HM.singleton SealCoin (mkCoin 0)) currencyAddrCoinMapMap
    where
        gatherBalanceByCurrency
            :: HashMap Currency Coin 
            -> Currency 
            -> AddrCoinMap 
            -> HashMap Currency Coin 
        gatherBalanceByCurrency currencyCoins currency addrCoins = 
            case (HM.lookup addr addrCoins) of
                Just amount -> HM.insertWith unsafeAddCoin currency amount currencyCoins
                Nothing     -> currencyCoins