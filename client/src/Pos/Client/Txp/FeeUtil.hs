{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Pure functions for create seal (inputs, outputs) pair as tx fee

module Pos.Client.Txp.FeeUtil
       ( prepareTxWithFeeInSeal
       ) where

import           Universum 
import           Control.Monad.Except (MonadError (throwError))
import qualified Data.Semigroup as S
import qualified Data.List.NonEmpty as NE

import           Pos.Client.Txp.Addresses (MonadAddresses (..))

import           Pos.Client.Txp.Util (TxCreator, TxRaw (..), TxOutputs, 
                                      PendingAddresses (..), InputPickingWay, TxError (..), 
                                      InputSelectionPolicy (..), 
                                      sumTxOutCoins, plainInputPicker, groupedInputPicker, 
                                      txToLinearFee, fixedToFee, createFakeTxFromRawTx, 
                                      withLinearFeePolicy, tcdInputSelectionPolicy)

import           Pos.Core (Address, TxSizeLinear (..),
                           integerToCoin, mkCoin, txSizeLinearMinValue, unsafeSubCoin, sealCurrency)
import           Pos.Crypto (ProtocolMagic)
import           Pos.Txp (TxFee (..), TxOut (..), TxIn (..), TxOutAux (..), Utxo, filterUtxoByCurrency)



-- | 基于预先生成的非Seal类型的Inputs和Outputs,计算以Seal作为币种的手续费,并组合在一起以TxRaw返回
-- | 输入的Utxo无需按Seal币过滤,函数内自带过滤
prepareTxWithFeeInSeal
    :: MonadAddresses m
    => ProtocolMagic
    -> PendingAddresses
    -> Utxo
    -> [(TxOut, TxIn)]
    -> TxOutputs
    -> TxCreator m TxRaw
prepareTxWithFeeInSeal pm pendingTx utxo nsInps nsOuts = 
        withLinearFeePolicy $ \linearPolicy ->
            stabilizeTxFeeInSeal pm pendingTx linearPolicy sealUtxo nsInps nsOuts
    where
        sealUtxo = filterUtxoByCurrency utxo sealCurrency

stabilizeTxFeeInSeal
    :: forall m
     . MonadAddresses m
    => ProtocolMagic
    -> PendingAddresses
    -> TxSizeLinear
    -> Utxo
    -> [(TxOut, TxIn)]
    -> TxOutputs
    -> TxCreator m TxRaw
stabilizeTxFeeInSeal pm pendingTx linearPolicy utxo nsInps nsOuts = do
    minFee <- fixedToFee (txSizeLinearMinValue linearPolicy)
    mtx <- stabilizeTxFeeDo (False, firstStageAttempts) minFee
    case mtx of
        Nothing -> throwError FailedToStabilize
        Just tx -> pure $ tx & \(S.Min (S.Arg _ txRaw)) -> txRaw
  where
    firstStageAttempts = 2 * length utxo + 5
    secondStageAttempts = 10

    stabilizeTxFeeDo :: (Bool, Int)
                     -> TxFee
                     -> TxCreator m $ Maybe (S.ArgMin TxFee TxRaw)
    stabilizeTxFeeDo (_, 0) _ = pure Nothing
    stabilizeTxFeeDo (isSecondStage, attempt) expectedFee = do
        fakeChangeAddr <- lift . lift $ getFakeChangeAddress
        txRaw <- prepareTxRawInSeal pendingTx utxo (makeFakeOutputs fakeChangeAddr) expectedFee nsInps nsOuts
        txMinFee <- txToLinearFee linearPolicy $
                    createFakeTxFromRawTx pm fakeChangeAddr txRaw

        let txRawWithFee = S.Min $ S.Arg expectedFee txRaw
        let iterateDo step = stabilizeTxFeeDo step txMinFee
        case expectedFee `compare` txMinFee of
            LT -> iterateDo (isSecondStage, attempt - 1)
            EQ -> pure (Just txRawWithFee)
            GT -> do
                let nextStep = (True, if isSecondStage then attempt - 1 else secondStageAttempts)
                futureRes <- iterateDo nextStep
                return $! Just txRawWithFee S.<> futureRes

makeFakeOutputs :: Address -> TxOutputs
makeFakeOutputs addr = (TxOutAux $ TxOut addr sealCurrency (mkCoin 0)) :| []

prepareTxRawInSeal
    :: Monad m
    => PendingAddresses
    -> Utxo
    -> TxOutputs -- fake outputs
    -> TxFee
    -> [(TxOut, TxIn)]
    -> TxOutputs
    -> TxCreator m TxRaw
prepareTxRawInSeal pendingTx utxo outputs fee nsInps nsOuts = do
    inputSelectionPolicy <- view tcdInputSelectionPolicy
    let inputPicker =
          case inputSelectionPolicy of
            OptimizeForHighThroughput -> plainInputPicker pendingTx
            OptimizeForSecurity       -> groupedInputPicker
    prepareTxRawInSealWithPicker inputPicker utxo outputs fee nsInps nsOuts

prepareTxRawInSealWithPicker
    :: Monad m
    => InputPickingWay
    -> Utxo
    -> TxOutputs -- fake outputs
    -> TxFee
    -> [(TxOut, TxIn)]
    -> TxOutputs
    -> TxCreator m TxRaw
prepareTxRawInSealWithPicker inputPicker utxo outputs (TxFee fee) nsInps nsOuts = do
    let moneyToSpent = fee
    futxo <- either throwError pure $ inputPicker utxo outputs moneyToSpent
    case nonEmpty futxo of
        Nothing       -> throwError $ GeneralTxError "Failed to prepare inputs!"
        Just inputsNE -> do
            totalTxAmount <- sumTxOuts $ map snd inputsNE
            let trInputs' = map formTxInputs inputsNE
            let trInputs = case nsInps of
                                [] -> trInputs'
                                _  -> trInputs' <> (NE.fromList nsInps)
                trRemainingMoney = totalTxAmount `unsafeSubCoin` moneyToSpent
            let trOutputs = nsOuts
            pure TxRaw {..}
  where
    sumTxOuts = either (throwError . GeneralTxError) pure .
        integerToCoin . sumTxOutCoins
    formTxInputs (inp, TxOutAux txOut) = (txOut, inp)
