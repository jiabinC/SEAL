{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Client.Txp.NetworkCert
       ( TxMode
       , prepareCertTx
       ) where

import           Universum

import           Pos.Client.Txp.Addresses (MonadAddresses (..))
import           Pos.Client.Txp.Balances (MonadBalances (..))
import           Pos.Client.Txp.Network (TxMode)
import           Pos.Client.Txp.Util (InputSelectionPolicy, PendingAddresses (..))
import           Pos.Client.Txp.UtilCert (createCertTx)
import           Pos.Communication.Message ()
import           Pos.Core (Address)
import           Pos.Core.Txp (TxIn, TxAux, TxOut, TxOutAux)
import           Pos.Crypto (ProtocolMagic, SafeSigner)
import           Pos.Util.Util (eitherToThrow)

prepareCertTx
    :: TxMode m
    => ProtocolMagic
    -> (Address -> Maybe SafeSigner)
    -> PendingAddresses
    -> InputSelectionPolicy
    -> NonEmpty Address
    -> [TxIn]
    -> NonEmpty TxOutAux
    -> AddrData m
    -> m (TxAux, NonEmpty TxOut)
prepareCertTx pm hdwSigners pendingAddrs inputSelectionPolicy addrs inputs outputs addrData = do
    utxo <- getOwnUtxos (toList addrs)
    eitherToThrow =<< createCertTx pm pendingAddrs inputSelectionPolicy utxo hdwSigners inputs outputs addrData