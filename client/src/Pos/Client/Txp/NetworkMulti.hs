{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Client.Txp.NetworkMulti
       ( TxMode
       , prepareMTxMulti
       ) where

import           Universum

import           Pos.Client.Txp.Addresses (MonadAddresses (..))
import           Pos.Client.Txp.Balances (MonadBalances (..))
import           Pos.Client.Txp.Network (TxMode)
import           Pos.Client.Txp.Util (InputSelectionPolicy, PendingAddresses (..))
import           Pos.Client.Txp.UtilMulti (createMTxMulti)
import           Pos.Communication.Message ()
import           Pos.Core (Address)
import           Pos.Core.Txp (TxAux (..), TxOut (..), TxOutAux (..))
import           Pos.Txp.Toil.Types (filterUtxoPlainOnly)
import           Pos.Crypto (ProtocolMagic, SafeSigner)
import           Pos.Util.Util (eitherToThrow)

prepareMTxMulti
    :: TxMode m
    => ProtocolMagic
    -> (Address -> Maybe SafeSigner)
    -> PendingAddresses
    -> InputSelectionPolicy
    -> NonEmpty Address
    -> NonEmpty TxOutAux
    -> AddrData m
    -> m (TxAux, NonEmpty TxOut)
prepareMTxMulti pm hdwSigners pendingAddrs inputSelectionPolicy addrs outputs addrData = do
    utxo <- getOwnUtxos (toList addrs)
    let plainUtxo = filterUtxoPlainOnly utxo -- reserve plain TxOut only
    eitherToThrow =<< createMTxMulti pm pendingAddrs inputSelectionPolicy plainUtxo hdwSigners outputs addrData