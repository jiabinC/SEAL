{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Functions on client types
module Pos.Wallet.Web.ClientTypes.Functions
      ( encToCId
      , addressToCId
      , cIdToAddress
      , mkCTx
      , toCUpdateInfo
      , addrMetaToAccount
      ) where

import           Universum

import           Control.Monad.Error.Class (throwError)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import           Data.Text (Text)
import           Formatting (build, sformat)

import           Pos.Client.Txp.History (TxHistoryEntry (..))
import           Pos.Core (Address, ChainDifficulty, decodeTextAddress, makePubKeyAddressBoot,
                           unsafeAddCoin, mkCoin, pattern SealCoin, sumCoins, unsafeSubCoin)
import           Pos.Core.Txp (Tx (..), TxOut (..), txOutAddress, txOutValue, isPlainTxOut, isCertTxOut)
import           Pos.Core.Update (BlockVersionData (..), BlockVersionModifier (..),
                                  UpdateProposal (..))
import           Pos.Crypto (EncryptedSecretKey, encToPublic)
import           Pos.Update.Poll.Types (ConfirmedProposalState (..), StakeholderVotes,
                                        isPositiveVote)
import           Pos.Util.Servant
import           Pos.Wallet.Web.ClientTypes.Instances ()
import           Pos.Wallet.Web.ClientTypes.Types (AccountId (..), Addr, CCoin, CCurrency, CUserCert, CHash (..),
                                                   CId (..), CPtxCondition (..), CTx (..), CTxMeta,
                                                   CUpdateInfo (..), CWAddressMeta (..))

-- TODO: this is not completely safe. If someone changes
-- implementation of Buildable Address. It should be probably more
-- safe to introduce `class PSSimplified` that would have the same
-- implementation has it is with Buildable Address but then person
-- will know it will probably change something for purescript.
-- | Transform Address into CId
addressToCId :: Address -> CId w
addressToCId = CId . CHash . sformat build

cIdToAddress :: CId w -> Either Text Address
cIdToAddress (CId (CHash h)) = decodeTextAddress h

-- TODO: pass extra information to this function and choose
-- distribution based on this information. Currently it's always
-- bootstrap era distribution.
encToCId :: EncryptedSecretKey -> CId w
encToCId = encodeCType . makePubKeyAddressBoot . encToPublic

mergeTxOuts :: [TxOut] -> [TxOut]
mergeTxOuts = map stick . NE.groupWith groupFunc
  where 
        stick :: NonEmpty TxOut -> TxOut
        stick outs@(TxOut{txOutAddress = addr, txOutCurrency = currency} :| _) =
            TxOut addr currency (foldl1 unsafeAddCoin $ fmap txOutValue outs)
        stick (TxOutUserCert{txOutAddress = addr, txOutCert = cert} :| _) =
            TxOutUserCert addr cert

        groupFunc :: TxOut -> TxOut
        groupFunc TxOut{..} = TxOut txOutAddress txOutCurrency (mkCoin 0) 
        groupFunc txout@TxOutUserCert{} = txout

mkCTx
    :: ChainDifficulty    -- ^ Current chain difficulty (to get confirmations)
    -> TxHistoryEntry     -- ^ Tx history entry
    -> CTxMeta            -- ^ Transaction metadata
    -> CPtxCondition      -- ^ State of resubmission
    -> (Address -> Bool) -- ^ Whether addresses belong to the wallet
    -> Either Text CTx
mkCTx diff THEntry {..} meta pc addrBelongsToWallet = do
    let isOurTxAddress = addrBelongsToWallet . txOutAddress
        ownInputs = filter isOurTxAddress inputs
        ownOutputs = filter isOurTxAddress outputs

    when (null ownInputs && null ownOutputs) $
        throwError "Transaction is irrelevant to given wallet!"

    let 
        plainInputs = filter isPlainTxOut inputs
        plainOutpus = filter isPlainTxOut outputs

        isInSeal = (==SealCoin) . txOutCurrency 
        sealInputs = filter isInSeal plainInputs 
        sealOutputs = filter isInSeal plainOutpus 

        sumMoney = sumCoins . map txOutValue
        sealOutgoing = sumMoney sealInputs
        sealIncoming = sumMoney sealOutputs
        ctIsOutgoing = sealOutgoing > sealIncoming

        ctIsLocal = length ownInputs == length inputs
                 && length ownOutputs == length outputs
        
        flatTxOuts = map (\TxOut{..} -> (txOutCurrency, txOutValue))
        plainInpCCM = foldl' (flip $ uncurry $ HM.insertWith unsafeAddCoin) HM.empty $ flatTxOuts plainInputs
        plainOutCCM = foldl' (flip $ uncurry $ HM.insertWith unsafeAddCoin) HM.empty $ flatTxOuts plainOutpus
        
        mapSubCoin big little = HM.mapWithKey (\c a -> unsafeSubCoin a (HM.lookupDefault (mkCoin 0) c little)) big
        ctAmount = map encodeCType $ NE.fromList $ HM.toList $
            if | ctIsLocal    -> plainInpCCM
               | ctIsOutgoing -> mapSubCoin plainInpCCM plainOutCCM
               | otherwise    -> mapSubCoin plainOutCCM plainInpCCM

    return CTx {..}
  where
    ctId = encodeCType _thTxId
    encodeTxOut :: TxOut -> (CId Addr, CCurrency, CCoin)
    encodeTxOut TxOut{..} = (encodeCType txOutAddress, encodeCType txOutCurrency, encodeCType txOutValue)
    encodeTxOut TxOutUserCert{} = error "Lost!"

    encodeTxOutUserCert :: TxOut -> (CId Addr, CUserCert)
    encodeTxOutUserCert TxOutUserCert{..} = (encodeCType txOutAddress, encodeCType txOutCert)
    encodeTxOutUserCert TxOut{} = error "Lost!"

    inputs = _thInputs
    outputs = toList $ _txOutputs _thTx

    ctInputs = map encodeTxOut $ mergeTxOuts $ filter isPlainTxOut _thInputs
    ctOutputs = map encodeTxOut $ filter isPlainTxOut outputs

    ctCertInputs = map encodeTxOutUserCert $ mergeTxOuts $ filter isCertTxOut _thInputs
    ctCertOutputs = map encodeTxOutUserCert $ filter isCertTxOut outputs

    ctConfirmations = maybe 0 fromIntegral $ (diff -) <$> _thDifficulty
    ctMeta = meta
    ctCondition = pc

addrMetaToAccount :: CWAddressMeta -> AccountId
addrMetaToAccount CWAddressMeta{..} = AccountId
    { aiWId  = cwamWId
    , aiIndex = cwamAccountIndex
    }

-- | Return counts of negative and positive votes
countVotes :: StakeholderVotes -> (Int, Int)
countVotes = foldl' counter (0, 0)
  where counter (n, m) vote = if isPositiveVote vote
                              then (n + 1, m)
                              else (n, m + 1)

-- | Creates 'CTUpdateInfo' from 'ConfirmedProposalState'
toCUpdateInfo :: BlockVersionData -> ConfirmedProposalState -> CUpdateInfo
toCUpdateInfo bvd ConfirmedProposalState {..} =
    let UnsafeUpdateProposal {..} = cpsUpdateProposal
        cuiSoftwareVersion  = upSoftwareVersion
        cuiBlockVesion      = upBlockVersion
        cuiScriptVersion    = fromMaybe (bvdScriptVersion bvd)
                                        (bvmScriptVersion upBlockVersionMod)
        cuiImplicit         = cpsImplicit
--        cuiProposed         = cpsProposed
--        cuiDecided          = cpsDecided
--        cuiConfirmed        = cpsConfirmed
--        cuiAdopted          = cpsAdopted
        (cuiVotesFor, cuiVotesAgainst) = countVotes cpsVotes
        cuiPositiveStake    = encodeCType cpsPositiveStake
        cuiNegativeStake    = encodeCType cpsNegativeStake
    in CUpdateInfo {..}
