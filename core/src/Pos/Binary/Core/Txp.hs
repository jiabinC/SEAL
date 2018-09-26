{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Binary serialization of core Txp types.

module Pos.Binary.Core.Txp
       (
       ) where

import           Universum

import qualified Data.ByteString.Lazy as LBS

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..), decodeKnownCborDataItem,
                                   decodeListLenCanonical, decodeUnknownCborDataItem,
                                   deriveIndexedBi, cborError,
                                   deriveSimpleBi, encodeKnownCborDataItem, encodeListLen,
                                   encodeUnknownCborDataItem, enforceSize, matchSize)
import           Pos.Binary.Core.Common ()
import           Pos.Binary.Core.Script ()
import           Pos.Binary.Merkle ()
import qualified Pos.Core.Common as Common
import qualified Pos.Core.Txp as T

----------------------------------------------------------------------------
-- Core
----------------------------------------------------------------------------

instance Bi T.TxIn where
    encode T.TxInUtxo{..} =
        encodeListLen 2 <>
        encode (0 :: Word8) <>
        encodeKnownCborDataItem (txInHash, txInIndex)
    encode T.TxInUserCert{..} = 
        encodeListLen 2 <>
        encode (1 :: Word8) <>
        encodeKnownCborDataItem (txInHash, txInIndex)
    encode T.TxInSealState{..} = 
        encodeListLen 2 <>
        encode (2 :: Word8) <>
        encodeKnownCborDataItem (txInHash, txInIndex, txInSealCert)
    encode (T.TxInUnknown tag bs) =
        encodeListLen 2 <>
        encode tag <>
        encodeUnknownCborDataItem (LBS.fromStrict bs)
    decode = do
        enforceSize "TxIn" 2
        tag <- decode @Word8
        case tag of
            0 -> uncurry T.TxInUtxo <$> decodeKnownCborDataItem
            1 -> uncurry T.TxInUserCert <$> decodeKnownCborDataItem
            2 -> (\(a,b,c) -> T.TxInSealState a b c) <$> decodeKnownCborDataItem
            _ -> T.TxInUnknown tag  <$> decodeUnknownCborDataItem

deriveIndexedBi ''T.UserCert [
    Cons 'T.UserCert [
        Field [| 0 :: Text |],
        Field [| 1 :: Text |]
    ]]

deriveSimpleBi ''T.SealCert [
    Cons 'T.GoldCoinIssueCert [
        Field [| T.issuedCoin :: Common.Coin|],
        Field [| T.reason :: Text|]
    ],
    Cons 'T.GoldDollarIssueCert [
        Field [| T.issuedCoin :: Common.Coin|],
        Field [| T.lockedGoldCoin :: Common.Coin|],
        Field [| T.reason :: Text|]
    ],
    Cons 'T.GoldDollarDestroyCert [
        Field [| T.destroyedCoin :: Common.Coin|],
        Field [| T.unlockedGoldCoin :: Common.Coin|],
        Field [| T.reason :: Text|]
    ]]

deriveSimpleBi ''T.SealState [
    Cons 'T.GoldCoinState [
        Field [| T.totalCoin :: Common.Coin|]
    ],
    Cons 'T.GoldDollarState [
        Field [| T.totalCoin :: Common.Coin|],
        Field [| T.totalLockedGoldCoin :: Common.Coin|]
    ]]

deriveSimpleBi ''T.TxOut [
    Cons 'T.TxOut [
        Field [| T.txOutAddress  :: Common.Address |],
        Field [| T.txOutCurrency :: Common.Currency |],
        Field [| T.txOutValue    :: Common.Coin    |]
    ],
    Cons 'T.TxOutUserCert [
        Field [| T.txOutAddress  :: Common.Address |],
        Field [| T.txOutCert     :: T.UserCert |]
    ],
    Cons 'T.TxOutSealState [
        Field [| T.txOutAddress  :: Common.Address |],
        Field [| T.txOutSealState :: T.SealState |]
    ]]

deriveSimpleBi ''T.TxOutAux [
    Cons 'T.TxOutAux [
        Field [| T.toaOut   :: T.TxOut |]
    ]]

instance Bi T.Tx where
    encode tx = encodeListLen 3
                <> encode (T._txInputs tx)
                <> encode (T._txOutputs tx)
                <> encode (T._txAttributes tx)
    decode = do
        enforceSize "Tx" 3
        T.UnsafeTx <$> decode <*> decode <*> decode

instance Bi T.TxInWitness where
    encode input = case input of
        T.PkWitness key sig         ->
            encodeListLen 2 <>
            encode (0 :: Word8) <>
            encodeKnownCborDataItem (key, sig)
        T.ScriptWitness val red     ->
            encodeListLen 2 <>
            encode (1 :: Word8) <>
            encodeKnownCborDataItem (val, red)
        T.RedeemWitness key sig     ->
            encodeListLen 2 <>
            encode (2 :: Word8) <>
            encodeKnownCborDataItem (key, sig)
        T.UnknownWitnessType tag bs ->
            encodeListLen 2 <>
            encode tag <>
            encodeUnknownCborDataItem (LBS.fromStrict bs)
    decode = do
        len <- decodeListLenCanonical
        tag <- decode @Word8
        case tag of
            0 -> do
                matchSize len "TxInWitness.PkWitness" 2
                uncurry T.PkWitness <$> decodeKnownCborDataItem
            1 -> do
                matchSize len "TxInWitness.ScriptWitness" 2
                uncurry T.ScriptWitness <$> decodeKnownCborDataItem
            2 -> do
                matchSize len "TxInWitness.RedeemWitness" 2
                uncurry T.RedeemWitness <$> decodeKnownCborDataItem
            _ -> do
                matchSize len "TxInWitness.UnknownWitnessType" 2
                T.UnknownWitnessType tag <$> decodeUnknownCborDataItem

instance Bi T.TxSigData where
    encode (T.TxSigData {..}) = encode txSigTxHash
    decode = T.TxSigData <$> decode

deriveSimpleBi ''T.TxAux [
    Cons 'T.TxAux [
        Field [| T.taTx           :: T.Tx             |],
        Field [| T.taWitness      :: T.TxWitness      |]
    ]]

instance Bi T.TxProof where
    encode proof =  encodeListLen 3
                 <> encode (T.txpNumber proof)
                 <> encode (T.txpRoot proof)
                 <> encode (T.txpWitnessesHash proof)
    decode = do
        enforceSize "TxProof" 3
        T.TxProof <$> decode <*>
                      decode <*>
                      decode

instance Bi T.TxPayload where
    encode T.UnsafeTxPayload {..} = encode $ zip (toList _txpTxs) _txpWitnesses
    decode = T.mkTxPayload <$> decode
