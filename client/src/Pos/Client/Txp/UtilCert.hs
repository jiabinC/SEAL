{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Client.Txp.UtilCert
       ( createCertTx
       ) where

import           Universum 
import           Control.Monad.Except (MonadError (throwError))
import qualified Data.Map as M

import           Pos.Client.Txp.Addresses (MonadAddresses (..))

import           Pos.Client.Txp.Util (TxOwnedInputs, TxCreator, TxCreateMode, TxRaw (..), TxOutputs, 
                                      PendingAddresses (..), TxError (..), 
                                      InputSelectionPolicy (..), TxWithSpendings,
                                      mkOutputsWithRem, makeMPubKeyTxAddrs, runTxCreator)
import           Pos.Client.Txp.FeeUtil (prepareTxWithFeeInSeal)

import           Pos.Core (Address, sealCurrency)
import           Pos.Crypto (ProtocolMagic, SafeSigner)
import           Pos.Txp (TxIn (..), TxOut (..), TxOutAux (..), Utxo)

createCertTx
    :: TxCreateMode m
    => ProtocolMagic
    -> PendingAddresses
    -> InputSelectionPolicy
    -> Utxo
    -> (Address -> Maybe SafeSigner)
    -> [TxIn]
    -> TxOutputs
    -> AddrData m
    -> m (Either TxError TxWithSpendings)
createCertTx pm pendingTx inputSelectionPolicy utxo hdwSigners inputs outputs addrData =
    runTxCreator inputSelectionPolicy $ do
        (inps, outs) <- prepareInpsOuts pm pendingTx utxo inputs outputs addrData
        txAux <- either throwError return $ creator inps outs
        pure (txAux, map fst inps)
  where
    getSigner address =
        note (SafeSignerNotFound address) $
        hdwSigners address
    
    creator = makeMPubKeyTxAddrs pm getSigner

prepareInpsOuts
    :: TxCreateMode m
    => ProtocolMagic
    -> PendingAddresses
    -> Utxo
    -> [TxIn]
    -> TxOutputs
    -> AddrData m
    -> TxCreator m (TxOwnedInputs TxOut, TxOutputs)
prepareInpsOuts pm pendingTx utxo inputs outputs addrData = do
    nsInps <- prepareTxOwnedInputs utxo inputs
    let nsOuts = outputs

    txRaw@TxRaw {..} <- prepareTxWithFeeInSeal pm pendingTx utxo nsInps nsOuts
    outputsWithRem <- mkOutputsWithRem addrData txRaw sealCurrency
    pure (trInputs, outputsWithRem)

prepareTxOwnedInputs 
    :: TxCreateMode m
    => Utxo 
    -> [TxIn] 
    -> TxCreator m [(TxOut, TxIn)]
prepareTxOwnedInputs utxo inputs = do
        mapM prepareOwned inputs 
    where 
        prepareOwned :: TxCreateMode m => TxIn -> TxCreator m (TxOut, TxIn)
        prepareOwned txIn = case (M.lookup txIn utxo) of
            Nothing           -> throwError $ GeneralTxError "Transaction not found in utxo!"
            Just TxOutAux{..} -> return (toaOut, txIn)