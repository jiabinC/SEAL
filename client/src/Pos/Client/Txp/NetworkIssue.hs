{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Client.Txp.NetworkIssue
       ( TxMode
       , prepareGoldIssue
       , prepareDollarIssue
       , prepareDollarDestroy
       ) where

import           Universum

import           Pos.Client.Txp.Addresses (MonadAddresses (..))
import           Pos.Client.Txp.Balances (MonadBalances (..))
import           Pos.Client.Txp.Network (TxMode)
import           Pos.Client.Txp.Util (InputSelectionPolicy, PendingAddresses (..))
import           Pos.Client.Txp.UtilIssue (createGoldIssueTx, createDollarIssueTx, createDollarDestroyTx)
import           Pos.Communication.Message ()
import           Pos.Core (Address, Coin)
import           Pos.Core.Txp (TxAux, TxOut)
import           Pos.Crypto (ProtocolMagic, SafeSigner)
import           Pos.Util.Util (eitherToThrow)

prepareGoldIssue
    :: TxMode m
    => ProtocolMagic
    -> (Address -> Maybe SafeSigner)
    -> PendingAddresses
    -> InputSelectionPolicy
    -> NonEmpty Address
    -> Coin
    -> Text
    -> AddrData m
    -> m (TxAux, NonEmpty TxOut)
prepareGoldIssue pm hdwSigners pendingAddrs inputSelectionPolicy addrs issuedGold reason addrData = do
    utxo <- getOwnUtxos (toList addrs)
    eitherToThrow =<< createGoldIssueTx pm pendingAddrs inputSelectionPolicy utxo hdwSigners issuedGold reason addrData

prepareDollarIssue
    :: TxMode m
    => ProtocolMagic
    -> (Address -> Maybe SafeSigner)
    -> PendingAddresses
    -> InputSelectionPolicy
    -> NonEmpty Address
    -> Coin
    -> Coin
    -> Text
    -> AddrData m
    -> m (TxAux, NonEmpty TxOut)
prepareDollarIssue pm hdwSigners pendingAddrs inputSelectionPolicy addrs issuedDollar lockedGold reason addrData = do
    utxo <- getOwnUtxos (toList addrs)
    eitherToThrow =<< createDollarIssueTx pm pendingAddrs inputSelectionPolicy utxo hdwSigners issuedDollar lockedGold reason addrData

prepareDollarDestroy
    :: TxMode m
    => ProtocolMagic
    -> (Address -> Maybe SafeSigner)
    -> PendingAddresses
    -> InputSelectionPolicy
    -> NonEmpty Address
    -> Coin
    -> Coin
    -> Text
    -> AddrData m
    -> m (TxAux, NonEmpty TxOut)
prepareDollarDestroy pm hdwSigners pendingAddrs inputSelectionPolicy addrs destroyedDollar unlockedGold reason addrData = do
    utxo <- getOwnUtxos (toList addrs)
    eitherToThrow =<< createDollarDestroyTx pm pendingAddrs inputSelectionPolicy utxo hdwSigners destroyedDollar unlockedGold reason addrData