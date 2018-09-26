{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Client.Txp.UtilIssue
       ( createGoldIssueTx
       , createDollarIssueTx
       , createDollarDestroyTx
       ) where

import           Universum 
import           Control.Monad.Except (MonadError (throwError))
import qualified Data.List.NonEmpty as NE

import           Pos.Client.Txp.Addresses (MonadAddresses (..))

import           Pos.Client.Txp.Util (TxOwnedInputs, TxCreator, TxCreateMode, TxRaw (..), TxOutputs, 
                                      PendingAddresses (..), TxError (..), 
                                      InputSelectionPolicy (..), TxWithSpendings,
                                      mkOutputsWithRem, makeMPubKeyTxAddrs, runTxCreator)
import           Pos.Client.Txp.UtilMulti(prepareInpsOutsWithoutFee)
import           Pos.Client.Txp.FeeUtil (prepareTxWithFeeInSeal)

import           Pos.Core (Address, Coin, Currency, pattern SealCoin, pattern GoldCoin, pattern GoldDollar, addCoin, subCoin)
import           Pos.Crypto (ProtocolMagic, SafeSigner)
import           Pos.Txp (TxIn (..), TxOut (..), TxAux, TxOutAux (..), SealState (..), SealCert (..), Utxo, 
                          getGoldCoinStateOnly, getGoldDollarStateOnly)

createGoldIssueTx
    :: TxCreateMode m
    => ProtocolMagic
    -> PendingAddresses
    -> InputSelectionPolicy
    -> Utxo
    -> (Address -> Maybe SafeSigner)
    -> Coin
    -> Text
    -> AddrData m
    -> m (Either TxError TxWithSpendings)
createGoldIssueTx pm pendingTx inputSelectionPolicy utxo hdwSigners issuedGold reason addrData =
    runTxCreator inputSelectionPolicy $ do
        (inps, outs) <- prepareInpsOutsForIssueGold pm pendingTx utxo issuedGold reason addrData
        txAux <- either throwError return $ creator inps outs
        pure (txAux, map fst inps)
  where
    creator = getCreator pm hdwSigners

getCreator 
    :: ProtocolMagic
    -> (Address -> Maybe SafeSigner)
    -> TxOwnedInputs TxOut
    -> TxOutputs
    -> Either TxError TxAux
getCreator pm hdwSigners = 
        makeMPubKeyTxAddrs pm getSigner
    where
        getSigner address =
            note (SafeSignerNotFound address) $
            hdwSigners address

prepareInpsOutsForIssueGold
    :: TxCreateMode m
    => ProtocolMagic
    -> PendingAddresses
    -> Utxo
    -> Coin
    -> Text
    -> AddrData m
    -> TxCreator m (TxOwnedInputs TxOut, TxOutputs)
prepareInpsOutsForIssueGold pm pendingTx utxo issuedGold reason addrData = do
        (gsTxIn, gsTxOutAux) <- either (throwError . GeneralTxError) pure $ getGoldCoinStateOnly utxo
        let stateInp = newStateInp gsTxIn
        stateOut <- newStateOut gsTxOutAux
        goldOut <- newGoldOut

        let nsInps = [(toaOut gsTxOutAux, stateInp)]
        let nsOuts = stateOut :| [goldOut]

        txRaw@TxRaw {..} <- prepareTxWithFeeInSeal pm pendingTx utxo nsInps nsOuts
        outputsWithRem <- mkOutputsWithRem addrData txRaw SealCoin
        pure (trInputs, outputsWithRem)
    where
        newStateInp gsTxIn = 
            let sealCert = GoldCoinIssueCert issuedGold reason
            in TxInSealState (txInHash gsTxIn) (txInIndex gsTxIn) sealCert
        
        newStateOut gsTxOutAux = do -- gsTxOutAux is TxOutAux TxOutSealState GoldCoinState
            let oldTotalCoin = totalCoin $ txOutSealState $ toaOut gsTxOutAux
            newTotalCoin <- case (addCoin oldTotalCoin issuedGold) of 
                                Nothing -> throwError $ 
                                               GeneralTxError $ "Add total gold: issued=" <> (show issuedGold) <> ", total=" <> (show oldTotalCoin) 
                                Just c  -> return c
            let sealState = GoldCoinState newTotalCoin

            rcvAddress <- lift . lift $ getNewAddress addrData
            return . TxOutAux $ TxOutSealState rcvAddress sealState
        
        newGoldOut = do
            rcvAddress <- lift . lift $ getNewAddress addrData
            pure . TxOutAux $ TxOut rcvAddress GoldCoin issuedGold

createDollarIssueTx
    :: TxCreateMode m
    => ProtocolMagic
    -> PendingAddresses
    -> InputSelectionPolicy
    -> Utxo
    -> (Address -> Maybe SafeSigner)
    -> Coin
    -> Coin
    -> Text
    -> AddrData m
    -> m (Either TxError TxWithSpendings)
createDollarIssueTx pm pendingTx inputSelectionPolicy utxo hdwSigners issuedDollar lockedGold reason addrData =
    runTxCreator inputSelectionPolicy $ do
        (inps, outs) <- prepareInpsOutsForIssueDollar pm pendingTx utxo issuedDollar lockedGold reason addrData
        txAux <- either throwError return $ creator inps outs
        pure (txAux, map fst inps)
  where
    creator = getCreator pm hdwSigners

prepareInpsOutsForIssueDollar
    :: TxCreateMode m
    => ProtocolMagic
    -> PendingAddresses
    -> Utxo
    -> Coin
    -> Coin
    -> Text
    -> AddrData m
    -> TxCreator m (TxOwnedInputs TxOut, TxOutputs)
prepareInpsOutsForIssueDollar pm pendingTx utxo issuedDollar lockedGold reason addrData = do
    (stateInps, stateOuts) <- prepareDollarStateInpsOutsForIssue utxo issuedDollar lockedGold reason addrData
    (plainInps, plainOuts) <- preparePlainInpsOuts pendingTx utxo GoldCoin lockedGold addrData

    let nsInps = NE.toList $ stateInps <> plainInps
    let nsOuts = NE.fromList $ (NE.toList stateOuts) <> plainOuts

    txRaw@TxRaw {..} <- prepareTxWithFeeInSeal pm pendingTx utxo nsInps nsOuts
    outputsWithRem <- mkOutputsWithRem addrData txRaw SealCoin
    pure (trInputs, outputsWithRem)

prepareDollarStateInpsOutsForIssue
    :: TxCreateMode m
    => Utxo
    -> Coin
    -> Coin
    -> Text
    -> AddrData m
    -> TxCreator m (TxOwnedInputs TxOut, TxOutputs)
prepareDollarStateInpsOutsForIssue utxo issuedDollar lockedGold reason addrData = do
        (gsTxIn, gsTxOutAux) <- either (throwError . GeneralTxError) pure $ getGoldDollarStateOnly utxo
        let stateInp = newStateInp gsTxIn
        stateOut <- newStateOut gsTxOutAux
        dollarOut <- newDollarOut

        let inps = (toaOut gsTxOutAux, stateInp) :| []
        let outs = stateOut :| [dollarOut]
        pure (inps, outs)
    where
        newStateInp gsTxIn = 
            let sealCert = GoldDollarIssueCert issuedDollar lockedGold reason
            in TxInSealState (txInHash gsTxIn) (txInIndex gsTxIn) sealCert
        
        newStateOut gsTxOutAux = do -- gsTxOutAux is TxOutAux TxOutSealState GoldDollarState
            let oldTotalCoin = totalCoin $ txOutSealState $ toaOut gsTxOutAux
            let oldTotalLockedGoldCoin = totalLockedGoldCoin $ txOutSealState $ toaOut gsTxOutAux
            newTotalCoin <- case (addCoin oldTotalCoin issuedDollar) of 
                                Nothing -> throwError $ 
                                               GeneralTxError $ "Add total dollar: issued=" <> (show issuedDollar) <> ", total=" <> (show oldTotalCoin) 
                                Just c  -> return c
            newTotalLockedGoldCoin <- case (addCoin oldTotalLockedGoldCoin lockedGold) of 
                                Nothing -> throwError $ 
                                               GeneralTxError $ "Add locked gold: locked=" <> (show lockedGold) <> ", total=" <> (show oldTotalLockedGoldCoin) 
                                Just c  -> return c

            let sealState = GoldDollarState newTotalCoin newTotalLockedGoldCoin

            rcvAddress <- lift . lift $ getNewAddress addrData
            return . TxOutAux $ TxOutSealState rcvAddress sealState
        
        newDollarOut = do
            rcvAddress <- lift . lift $ getNewAddress addrData
            pure . TxOutAux $ TxOut rcvAddress GoldDollar issuedDollar

createDollarDestroyTx
    :: TxCreateMode m
    => ProtocolMagic
    -> PendingAddresses
    -> InputSelectionPolicy
    -> Utxo
    -> (Address -> Maybe SafeSigner)
    -> Coin
    -> Coin
    -> Text
    -> AddrData m
    -> m (Either TxError TxWithSpendings)
createDollarDestroyTx pm pendingTx inputSelectionPolicy utxo hdwSigners destroyedDollar unlockedGold reason addrData =
    runTxCreator inputSelectionPolicy $ do
        (inps, outs) <- prepareInpsOutsForDestroyDollar pm pendingTx utxo destroyedDollar unlockedGold reason addrData
        txAux <- either throwError return $ creator inps outs
        pure (txAux, map fst inps)
  where
    creator = getCreator pm hdwSigners

prepareInpsOutsForDestroyDollar
    :: TxCreateMode m
    => ProtocolMagic
    -> PendingAddresses
    -> Utxo
    -> Coin
    -> Coin
    -> Text
    -> AddrData m
    -> TxCreator m (TxOwnedInputs TxOut, TxOutputs)
prepareInpsOutsForDestroyDollar pm pendingTx utxo destroyedDollar unlockedGold reason addrData = do
    (stateInps, stateOuts) <- prepareDollarStateInpsOutsForDestroy utxo destroyedDollar unlockedGold reason addrData
    (plainInps, plainOuts) <- preparePlainInpsOuts pendingTx utxo GoldDollar destroyedDollar addrData

    let nsInps = NE.toList $ stateInps <> plainInps
    let nsOuts = NE.fromList $ (NE.toList stateOuts) <> plainOuts

    txRaw@TxRaw {..} <- prepareTxWithFeeInSeal pm pendingTx utxo nsInps nsOuts
    outputsWithRem <- mkOutputsWithRem addrData txRaw SealCoin
    pure (trInputs, outputsWithRem)

prepareDollarStateInpsOutsForDestroy
    :: TxCreateMode m
    => Utxo
    -> Coin
    -> Coin
    -> Text
    -> AddrData m
    -> TxCreator m (TxOwnedInputs TxOut, TxOutputs)
prepareDollarStateInpsOutsForDestroy utxo destroyedDollar unlockedGold reason addrData = do
        (gsTxIn, gsTxOutAux) <- either (throwError . GeneralTxError) pure $ getGoldDollarStateOnly utxo
        let stateInp = newStateInp gsTxIn
        stateOut <- newStateOut gsTxOutAux
        goldOut <- newGoldOut

        let inps = (toaOut gsTxOutAux, stateInp) :| []
        let outs = stateOut :| [goldOut]
        pure (inps, outs)
    where
        newStateInp gsTxIn = 
            let sealCert = GoldDollarDestroyCert destroyedDollar unlockedGold reason
            in TxInSealState (txInHash gsTxIn) (txInIndex gsTxIn) sealCert
        
        newStateOut gsTxOutAux = do -- gsTxOutAux is TxOutAux TxOutSealState GoldDollarState
            let oldTotalCoin = totalCoin $ txOutSealState $ toaOut gsTxOutAux
            let oldTotalLockedGoldCoin = totalLockedGoldCoin $ txOutSealState $ toaOut gsTxOutAux
            newTotalCoin <- case (subCoin oldTotalCoin destroyedDollar) of 
                                Nothing -> throwError $ 
                                               GeneralTxError $ "Substract total dollar: destroyed=" <> (show destroyedDollar) <> ", total=" <> (show oldTotalCoin) 
                                Just c  -> return c
            newTotalLockedGoldCoin <- case (subCoin oldTotalLockedGoldCoin unlockedGold) of 
                                Nothing -> throwError $
                                               GeneralTxError $ "Substract locked gold: unlocked=" <> (show destroyedDollar) <> ", total=" <> (show oldTotalCoin) 
                                Just c  -> return c

            let sealState = GoldDollarState newTotalCoin newTotalLockedGoldCoin

            rcvAddress <- lift . lift $ getNewAddress addrData
            return . TxOutAux $ TxOutSealState rcvAddress sealState

        newGoldOut = do
            rcvAddress <- lift . lift $ getNewAddress addrData
            pure . TxOutAux $ TxOut rcvAddress GoldCoin unlockedGold

preparePlainInpsOuts        
    :: TxCreateMode m
    => PendingAddresses
    -> Utxo
    -> Currency
    -> Coin
    -> AddrData m
    -> TxCreator m (TxOwnedInputs TxOut, [TxOutAux])
preparePlainInpsOuts pendingTx utxo currency amount addrData = do
        fakeTxOut <- newFakeTxOut
        let fakeTxOutAux = TxOutAux fakeTxOut

        let fakeOutputs = fakeTxOutAux :| []
        (inps, outs) <- prepareInpsOutsWithoutFee pendingTx utxo fakeOutputs addrData currency
        let outsWithoutFake = NE.filter (/=fakeTxOutAux) outs
        pure (inps, outsWithoutFake)
    where 
        newFakeTxOut = do
            wastedAddress <- lift . lift $ getNewAddress addrData
            return $ TxOut wastedAddress currency amount
