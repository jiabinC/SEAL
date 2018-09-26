{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Pure functions for operations with transactions
-- !!! 注意: 这个文件中中出现的Utxo都已经过滤为只剩下币类交易 !!!

module Pos.Client.Txp.UtilMulti
       ( createMTxMulti
       , prepareInpsOutsWithoutFee
       ) where

import           Universum 
import           Control.Monad.Except (MonadError (throwError))
import qualified Data.List.NonEmpty as NE

import           Pos.Client.Txp.Addresses (MonadAddresses (..))

import           Pos.Client.Txp.Util (TxOwnedInputs, TxCreator, TxCreateMode, TxRaw (..), TxOutputs, 
                                      PendingAddresses (..), TxError (..), 
                                      InputSelectionPolicy (..), TxWithSpendings,
                                      plainInputPicker, groupedInputPicker, mkOutputsWithRem,
                                      prepareTxRawWithPicker, makeMPubKeyTxAddrs, runTxCreator, 
                                      tcdInputSelectionPolicy)
import           Pos.Client.Txp.FeeUtil (prepareTxWithFeeInSeal)

import           Pos.Core (Address, Currency, mkCoin, sealCurrency)
import           Pos.Crypto (ProtocolMagic, SafeSigner)
import           Pos.Txp (TxAux (..), TxFee (..), TxOut (..), TxOutAux (..), Utxo, filterUtxoByCurrency)

-- | Make a multi-transaction using given secret key and info for outputs.
-- Currently used for HD wallets only, thus `HDAddressPayload` is required
createMTxMulti
    :: TxCreateMode m
    => ProtocolMagic
    -> PendingAddresses
    -> InputSelectionPolicy
    -> Utxo
    -> (Address -> Maybe SafeSigner)
    -> TxOutputs
    -> AddrData m
    -> m (Either TxError TxWithSpendings)
createMTxMulti pm pendingTx inputSelectionPolicy utxo hdwSigners outputs addrData =
    createGenericTxMulti pm pendingTx (makeMPubKeyTxAddrs pm getSigner) inputSelectionPolicy utxo outputs addrData
  where
    getSigner address =
        note (SafeSignerNotFound address) $
        hdwSigners address

createGenericTxMulti
    :: TxCreateMode m
    => ProtocolMagic
    -> PendingAddresses
    -> (TxOwnedInputs TxOut -> TxOutputs -> Either TxError TxAux)
    -> InputSelectionPolicy
    -> Utxo
    -> TxOutputs
    -> AddrData m
    -> m (Either TxError TxWithSpendings)
createGenericTxMulti pm pendingTx creator inputSelectionPolicy utxo outputs addrData =
    runTxCreator inputSelectionPolicy $ do
        (inps, outs) <- prepareInpsOutsMulti pm pendingTx utxo outputs addrData
        txAux <- either throwError return $ creator inps outs
        pure (txAux, map fst inps)

prepareInpsOutsMulti
    :: TxCreateMode m
    => ProtocolMagic
    -> PendingAddresses
    -> Utxo
    -> TxOutputs
    -> AddrData m
    -> TxCreator m (TxOwnedInputs TxOut, TxOutputs)
prepareInpsOutsMulti pm pendingTx utxo outputs addrData = do
        (nsInps, nsOuts) <- prepareInpsOutsWithoutFeeMerged pendingTx utxo outputs addrData currencies
        txRaw@TxRaw {..} <- prepareTxWithFeeInSeal pm pendingTx utxo (NE.toList nsInps) nsOuts
        outputsWithRem <- mkOutputsWithRem addrData txRaw sealCurrency
        pure (trInputs, outputsWithRem)
    where  
        currencies = NE.nub $ NE.map (txOutCurrency . toaOut) outputs

prepareInpsOutsWithoutFeeMerged
    :: TxCreateMode m
    => PendingAddresses
    -> Utxo
    -> TxOutputs
    -> AddrData m
    -> NonEmpty Currency
    -> TxCreator m (TxOwnedInputs TxOut, TxOutputs)
prepareInpsOutsWithoutFeeMerged pendingTx utxo outputs addrData currencies = do
        inpsOutsPairs <- mapM (prepareInpsOutsWithoutFee pendingTx utxo outputs addrData) (NE.toList currencies)
        pure $ foldl1 (<>) inpsOutsPairs

prepareInpsOutsWithoutFee
    :: TxCreateMode m
    => PendingAddresses
    -> Utxo
    -> TxOutputs
    -> AddrData m
    -> Currency
    -> TxCreator m (TxOwnedInputs TxOut, TxOutputs)
prepareInpsOutsWithoutFee pendingTx utxo outputs addrData currency = do
        txRaw@TxRaw {..} <- prepareTxRawWithoutFee pendingTx filteredUtxo filteredOutputs
        outputsWithRem <- mkOutputsWithRem addrData txRaw currency
        pure (trInputs, outputsWithRem)
    where 
        filteredUtxo = filterUtxoByCurrency utxo currency
        filteredOutputs = NE.fromList $ NE.filter ((== currency) . txOutCurrency . toaOut) outputs

prepareTxRawWithoutFee
    :: Monad m
    => PendingAddresses
    -> Utxo
    -> TxOutputs
    -> TxCreator m TxRaw
prepareTxRawWithoutFee pendingTx utxo outputs = do
    inputSelectionPolicy <- view tcdInputSelectionPolicy
    let inputPicker =
          case inputSelectionPolicy of
            OptimizeForHighThroughput -> plainInputPicker pendingTx
            OptimizeForSecurity       -> groupedInputPicker
    prepareTxRawWithPicker inputPicker utxo outputs (TxFee $ mkCoin 0)

-- | Compute, how much fees we should pay to send money to given
-- outputs
-- computeTxFeeMulti
--     :: MonadAddresses m
--     => ProtocolMagic
--     -> PendingAddresses
--     -> Utxo
--     -> TxOutputs
--     -> TxCreator m TxFee
-- computeTxFeeMulti pm pendingTx utxo outputs = do
--     TxRaw {..} <- prepareTxWithFee pm pendingTx utxo outputs
--     let outAmount = sumTxOutCoins trOutputs
--         inAmount = sumCoins $ map (txOutValue . fst) trInputs
--         remaining = coinToInteger trRemainingMoney
--     integerToFee $ inAmount - outAmount - remaining