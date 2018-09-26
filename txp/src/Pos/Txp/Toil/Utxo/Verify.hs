module Pos.Txp.Toil.Utxo.Verify 
    ( runVerifyM
    , verifyTx) where

import           Universum

import           Control.Arrow ((***))
import           Control.Monad.Except (Except, runExcept, throwError)
import           Data.List (partition)
-- import           Formatting (build, sformat, (%))

import           Pos.Binary.Core ()
import           Pos.Core (integerToCoin, sumCoins)
import           Pos.Core.Common (Coin, pattern SealCoin, pattern GoldCoin, pattern GoldDollar)
import           Pos.Core.Txp (TxIn (..), TxOut (..), TxOutAux (..), matchCurrency, 
                               isCertTxOut, SealState(..), isSealStateTxOut, SealCert(..))
import           Pos.Txp.Toil.Failure (ToilVerFailure (..))
import           Pos.Txp.Toil.Types (TxFee (..))

  -- 交易验证Monad
data VerifyState = VerifyState {
  vsInputs ::  [(TxIn, TxOutAux)]
, vsOutputs :: [TxOut]
}

type VerifyM = StateT VerifyState (Except ToilVerFailure)

runVerifyM :: 
       NonEmpty (TxIn, TxOutAux)
    -> NonEmpty TxOut
    -> VerifyM a
    -> Either ToilVerFailure a
runVerifyM resolvedInputs outputs action = 
    let inputs = toList resolvedInputs
    in runExcept $ evalStateT action (VerifyState inputs (toList outputs))

verifyTx :: VerifyM TxFee
verifyTx = do
    verifyUserCert                      -- user cert 校验
    txfee <- verifySumOfSeal            -- Seal币校验
    verifySealState                     -- 
    verifyUnknown                       -- 
    return txfee

filterInputs :: (TxOut -> Bool) -> VerifyM [(TxIn, TxOut)]
filterInputs f = do
    vs <- get
    let (rets, inputs) = partition (f . toaOut . snd) $ vsInputs vs
    put $ vs {vsInputs = inputs}
    return $ map (\(txin, txoutaux) -> (txin, toaOut txoutaux)) rets

filterOutputs :: (TxOut -> Bool) -> VerifyM [TxOut]
filterOutputs f = do
    vs <- get
    let (rets, outputs) = partition f $ vsOutputs vs
    put $ vs {vsOutputs = outputs}
    return rets

verifyUnknown :: VerifyM ()
verifyUnknown = do
    VerifyState inputs outputs <- get
    unless (null inputs) $ inconsistentTxAux "unknown inputs"
    unless (null outputs) $ inconsistentTxAux "unknown outputs"

verifyUserCert :: VerifyM ()
verifyUserCert = do
    _ <- filterInputs isCertTxOut
    _ <- filterOutputs isCertTxOut
    return ()
    

verifySumOfSeal :: VerifyM TxFee
verifySumOfSeal = do
    inputs <- map (txOutValue . snd) <$> filterInputs (matchCurrency SealCoin)
    outputs <- map txOutValue <$> filterOutputs (matchCurrency SealCoin)
    let mTxFee = TxFee <$> rightToMaybe (integerToCoin (inpSum - outSum))
        outSum = sumCoins outputs 
        inpSum = sumCoins inputs

    case mTxFee of
        Nothing -> throwError $ ToilOutGreaterThanIn inpSum outSum
        Just txFee ->
            return txFee
    -- | otherwise = inconsistentTxAux "unknown TxOut"
 
inconsistentTxAux :: Text -> VerifyM a
inconsistentTxAux = throwError . ToilInconsistentTxAux

-- 校验两组coin的总和是否相等
checkCoinsEqual :: [Coin] -> [Coin] -> Bool
checkCoinsEqual  coins1 coins2 = either (const False) id $ do
    sum1 <- integerToCoin $ sumCoins coins1
    sum2 <- integerToCoin $ sumCoins coins2
    return $ sum1 == sum2

-- 根据相应的凭证校验GoldCoin和GoldDollar总额是否匹配
verifySealCert :: Maybe SealCert -> VerifyM ()
verifySealCert mcert = do
    inGoldCoin <- map (txOutValue . snd) <$> filterInputs (matchCurrency GoldCoin)
    outGoldCoin <- map txOutValue <$> filterOutputs (matchCurrency GoldCoin)

    inGoldDollar <- map (txOutValue . snd) <$> filterInputs (matchCurrency GoldDollar)
    outGoldDollar <- map txOutValue <$> filterOutputs (matchCurrency GoldDollar)

    let go (Just GoldCoinIssueCert{..}) = do
            unless (checkCoinsEqual (issuedCoin : inGoldCoin) outGoldCoin) $
                inconsistentTxAux "GoldCoin not equal"
            unless (checkCoinsEqual inGoldDollar outGoldDollar) $
                inconsistentTxAux "GoldDollar not equal"
        
        go (Just GoldDollarIssueCert{..}) = do
            unless (checkCoinsEqual inGoldCoin (lockedGoldCoin : outGoldCoin)) $
                inconsistentTxAux "GoldCoin not equal"
            unless (checkCoinsEqual (issuedCoin : inGoldDollar) outGoldDollar) $
                inconsistentTxAux "GoldDollar not equal"

        go (Just GoldDollarDestroyCert{..}) = do
            unless (checkCoinsEqual (unlockedGoldCoin : inGoldCoin) outGoldCoin) $
                inconsistentTxAux "GoldCoin not equal"
            unless (checkCoinsEqual inGoldDollar (destroyedCoin : outGoldDollar)) $
                inconsistentTxAux "GoldDollar not equal"

        go Nothing = do
            unless (checkCoinsEqual inGoldCoin outGoldCoin) $
                inconsistentTxAux "GoldCoin not equal"
            unless (checkCoinsEqual inGoldDollar outGoldDollar) $
                inconsistentTxAux "GoldDollar not equal"

    go mcert

verifySealState :: VerifyM ()
verifySealState = do
    inStates <- map (txInSealCert *** txOutSealState) <$> filterInputs isSealStateTxOut
    outStates <- map txOutSealState <$> filterOutputs isSealStateTxOut

    go inStates outStates
    where
        stateError = inconsistentTxAux "invalid seal state output!"
        
        go [(cert, GoldCoinState c1)] [(GoldCoinState c2)] = handle cert
            where
                -- 黄金币发行
                handle GoldCoinIssueCert {..} = do
                    unless (checkCoinsEqual [c1, issuedCoin] [c2]) $
                        inconsistentTxAux "cert GoldCoin not equal"
                    verifySealCert $ Just cert
                handle _ = inconsistentTxAux "invalid seal cert"
        
        go [(cert, GoldDollarState total1 locked1)] [(GoldDollarState total2 locked2)] = handle cert
            where
                handle GoldDollarIssueCert{..} = do
                    unless (checkCoinsEqual [total1, issuedCoin] [total2]) $
                        inconsistentTxAux "cert GoldDollar not equal"
                    unless (checkCoinsEqual [locked1, lockedGoldCoin] [locked2]) $
                        inconsistentTxAux "GoldDollarIssueCert lockedGoldCoin invalid"
                    verifySealCert $ Just cert

                handle GoldDollarDestroyCert{..} = do
                    unless (checkCoinsEqual [total2, destroyedCoin] [total1]) $
                        inconsistentTxAux "invalid GoldDollarDestroyCert"
                    unless (checkCoinsEqual [locked1] [unlockedGoldCoin, locked2]) $
                        inconsistentTxAux "GoldDollarIssueCert lockedGoldCoin invalid"
                    verifySealCert $ Just cert

                handle _ = inconsistentTxAux "unmatched cert"
            
        go [] [] = verifySealCert Nothing

        go _ _ = stateError
