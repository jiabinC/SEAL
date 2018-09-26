{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Muiltiple currency's transaction creation and fees

module Pos.Wallet.Web.Methods.PaymentCert
       ( newCert
       ) where

import           Universum

import           Control.Monad.Except (runExcept)
import qualified Data.Map as M
import           Data.Time.Units (Second)
import           Servant.Server (err403, err405, errReasonPhrase)
import           System.Wlog (logDebug)

import           Pos.Client.KeyStorage (getSecretKeys)
import           Pos.Client.Txp.Addresses (MonadAddresses (..))
import           Pos.Client.Txp.History (TxHistoryEntry (..))
import           Pos.Client.Txp.NetworkCert (prepareCertTx)
import           Pos.Client.Txp.Util (InputSelectionPolicy (..), TxCreateMode)
import           Pos.Configuration (walletTxCreationDisabled)
import           Pos.Core (Address, TxIn (..), TxAux (..), TxOutAux (..), TxOut (..),
                           UserCert (..),  getCurrentTimestamp)
import           Pos.Core.Txp (_txOutputs)
import           Pos.Crypto (PassPhrase, ProtocolMagic, SafeSigner, ShouldCheckPassphrase (..),
                             checkPassMatches, hash, withSafeSignerUnsafe)
import           Pos.Util.Servant (FromCType (..))
import           Pos.Wallet.Aeson.ClientTypes ()
import           Pos.Wallet.Aeson.WalletBackup ()
import           Pos.Wallet.Web.Account (getSKByAddressPure, getSKById)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), CTx (..), NewCert (..))
import           Pos.Wallet.Web.Error (WalletError (..))
import           Pos.Wallet.Web.Methods.Payment (MoneySource (..), notFasterThan, getSomeMoneySourceAccount,
                                                 getMoneySourceWallet, getMoneySourceAddresses)
import           Pos.Wallet.Web.Methods.History (addHistoryTxMeta, constructCTx,
                                                 getCurChainDifficulty)
import           Pos.Wallet.Web.Methods.Txp (MonadWalletTxFull, getPendingAddresses, rewrapTxError, submitAndSaveNewPtx)
import           Pos.Wallet.Web.Pending (mkPendingTx)
import           Pos.Wallet.Web.State (AddressLookupMode (Ever), HasWAddressMeta (..), 
                                       askWalletDB, getWalletSnapshot, isWalletRestoring)
import           Pos.Wallet.Web.Util (getWalletAddrsDetector)


newCert
    :: (MonadWalletTxFull ctx m)
    => ProtocolMagic
    -> (TxAux -> m Bool)
    -> PassPhrase
    -> AccountId
    -> NewCert
    -- -> InputSelectionPolicy
    -> m CTx
newCert pm submitTx passphrase srcAccount cert = do
    notFasterThan (6 :: Second) $ do
      sendCert
          pm
          submitTx
          passphrase
          (AccountMoneySource srcAccount)
          cert
          OptimizeForHighThroughput

sendCert
    :: (MonadWalletTxFull ctx m)
    => ProtocolMagic
    -> (TxAux -> m Bool)
    -> PassPhrase
    -> MoneySource
    -> NewCert
    -> InputSelectionPolicy
    -> m CTx
sendCert pm submitTx passphrase moneySource cert policy = do
    db <- askWalletDB
    ws <- getWalletSnapshot db
    when walletTxCreationDisabled $
        throwM err405
        { errReasonPhrase = "Transaction creation is disabled by configuration!"
        }

    let srcWallet = getMoneySourceWallet moneySource
    when (isWalletRestoring ws srcWallet) $
        throwM err403
        { errReasonPhrase = "Transaction creation is disabled when the wallet is restoring."
        }
    rootSk <- getSKById srcWallet
    checkPassMatches passphrase rootSk `whenNothing`
        throwM (RequestError "Passphrase doesn't match")

    addrMetas' <- getMoneySourceAddresses ws moneySource
    addrMetas <- nonEmpty addrMetas' `whenNothing`
        throwM (RequestError "Given money source has no addresses!")

    let srcAddrs = map (view wamAddress) addrMetas

    logDebug "sendCert: processed addrs"

    let metasAndAddresses = M.fromList $ zip (toList srcAddrs) (toList addrMetas)
    allSecrets <- getSecretKeys

    let
        getSigner :: Address -> Maybe SafeSigner
        getSigner addr = do
          addrMeta <- M.lookup addr metasAndAddresses
          sk <- rightToMaybe . runExcept $
              getSKByAddressPure allSecrets (ShouldCheckPassphrase False) passphrase addrMeta
          withSafeSignerUnsafe sk (pure passphrase) pure

    relatedAccount <- getSomeMoneySourceAccount ws moneySource

    let addrData = (relatedAccount, passphrase)
    (inputs, outputs) <- certToInpsOuts cert addrData

    let pendingAddrs = getPendingAddresses ws policy
    th <- rewrapTxError "Cannot send transaction" $ do
        (txAux, inpTxOuts') <-
            prepareCertTx pm getSigner pendingAddrs policy srcAddrs inputs outputs addrData

        ts <- Just <$> getCurrentTimestamp
        let tx = taTx txAux
            txHash = hash tx
            inpTxOuts = toList inpTxOuts'
            dstAddrs  = map txOutAddress . toList $
                        _txOutputs tx
            th = THEntry txHash tx Nothing inpTxOuts dstAddrs ts
        ptx <- mkPendingTx ws srcWallet txHash txAux th

        th <$ submitAndSaveNewPtx pm db submitTx ptx

    -- We add TxHistoryEntry's meta created by us in advance
    -- to make TxHistoryEntry in CTx consistent with entry in history.
    _ <- addHistoryTxMeta db srcWallet th
    diff <- getCurChainDifficulty
    ws' <- getWalletSnapshot db
    let srcWalletAddrsDetector = getWalletAddrsDetector ws' Ever srcWallet

    logDebug "sendCert: constructing response"
    fst <$> constructCTx ws' srcWallet srcWalletAddrsDetector diff th

certToInpsOuts
    :: TxCreateMode m
    => NewCert
    -> AddrData m
    -> m ([TxIn], NonEmpty TxOutAux)
certToInpsOuts NewCert {..} addrData = do
        inputs <- concat <$> mapM conv2TxIn ncInputs
        outputs <- mapM conv2TxOutAux ncOutputs
        pure $ (inputs, outputs)
    where
        conv2TxIn (cTxId, index) = case (decodeCType cTxId) of
            Left _   -> pure [] --throwError $ GeneralTxError err  -- TODO xiaolie throw runtime error
            Right txId -> pure $ [TxInUserCert txId index ]
        
        conv2TxOutAux (t1, t2) = do 
            newAddr <- getNewAddress addrData
            pure . TxOutAux $ TxOutUserCert newAddr (UserCert t1 t2)
