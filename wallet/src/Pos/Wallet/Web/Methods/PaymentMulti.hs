{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Muiltiple currency's transaction creation and fees

module Pos.Wallet.Web.Methods.PaymentMulti
       ( newPaymentMulti
       , newPaymentBatchMulti
       ) where

import           Universum

import           Control.Monad.Except (runExcept)
import qualified Data.Map as M
import           Data.Time.Units (Second)
import           Servant.Server (err403, err405, errReasonPhrase)
import           System.Wlog (logDebug)

import           Pos.Client.KeyStorage (getSecretKeys)
import           Pos.Client.Txp.History (TxHistoryEntry (..))
import           Pos.Client.Txp.NetworkMulti (prepareMTxMulti)
import           Pos.Client.Txp.Util (InputSelectionPolicy (..))
import           Pos.Configuration (walletTxCreationDisabled)
import           Pos.Core (Address, Coin, Currency, TxAux (..), TxOut (..),
                           getCurrentTimestamp, sealCurrency)
import           Pos.Core.Txp (_txOutputs)
import           Pos.Crypto (PassPhrase, ProtocolMagic, SafeSigner, ShouldCheckPassphrase (..),
                             checkPassMatches, hash, withSafeSignerUnsafe)
import           Pos.Wallet.Aeson.ClientTypes ()
import           Pos.Wallet.Aeson.WalletBackup ()
import           Pos.Wallet.Web.Account (getSKByAddressPure, getSKById)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), Addr, CId, CTx (..), NewBatchPaymentMulti (..))
import           Pos.Wallet.Web.Error (WalletError (..))
import           Pos.Wallet.Web.Methods.Payment (MoneySource (..), notFasterThan, getSomeMoneySourceAccount,
                                                 getMoneySourceWallet, getMoneySourceAddresses, newPayment)
import           Pos.Wallet.Web.Methods.History (addHistoryTxMeta, constructCTx,
                                                 getCurChainDifficulty)
import           Pos.Wallet.Web.Methods.Txp (MonadWalletTxFull, coinDistrToOutputs,
                                             getPendingAddresses, rewrapTxError,
                                             submitAndSaveNewPtx)
import           Pos.Wallet.Web.Pending (mkPendingTx)
import           Pos.Wallet.Web.State (AddressLookupMode (Ever), HasWAddressMeta (..), 
                                       askWalletDB, getWalletSnapshot, isWalletRestoring)
import           Pos.Wallet.Web.Util (decodeCTypeOrFail, getWalletAddrsDetector)


newPaymentMulti
    :: MonadWalletTxFull ctx m
    => ProtocolMagic
    -> (TxAux -> m Bool)
    -> PassPhrase
    -> AccountId
    -> CId Addr
    -> Currency
    -> Coin
    -> InputSelectionPolicy
    -> m CTx
newPaymentMulti pm submitTx passphrase srcAccount dstAddress currency coin policy 
    | currency == sealCurrency = newPayment pm submitTx passphrase srcAccount dstAddress coin policy
    | otherwise =
        -- This is done for two reasons:
        -- 1. In order not to overflow relay.
        -- 2. To let other things (e. g. block processing) happen if
        -- `newPayment`s are done continuously.
        notFasterThan (6 :: Second) $ do
        sendMoney
            pm
            submitTx
            passphrase
            (AccountMoneySource srcAccount)
            (one (dstAddress, currency, coin))
            policy

newPaymentBatchMulti
    :: MonadWalletTxFull ctx m
    => ProtocolMagic
    -> (TxAux -> m Bool)
    -> PassPhrase
    -> NewBatchPaymentMulti
    -> m CTx
newPaymentBatchMulti pm submitTx passphrase NewBatchPaymentMulti {..} = do
    src <- decodeCTypeOrFail npbmFrom
    notFasterThan (6 :: Second) $ do
      sendMoney
          pm
          submitTx
          passphrase
          (AccountMoneySource src)
          npbmTo
          npbmInputSelectionPolicy

sendMoney
    :: (MonadWalletTxFull ctx m)
    => ProtocolMagic
    -> (TxAux -> m Bool)
    -> PassPhrase
    -> MoneySource
    -> NonEmpty (CId Addr, Currency, Coin)
    -> InputSelectionPolicy
    -> m CTx
sendMoney pm submitTx passphrase moneySource dstDistr policy = do
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

    logDebug "sendMoney: processed addrs"

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
    outputs <- coinDistrToOutputs dstDistr
    let pendingAddrs = getPendingAddresses ws policy
    th <- rewrapTxError "Cannot send transaction" $ do
        (txAux, inpTxOuts') <-
            prepareMTxMulti pm getSigner pendingAddrs policy srcAddrs outputs (relatedAccount, passphrase)

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

    logDebug "sendMoney(m): constructing response"
    fst <$> constructCTx ws' srcWallet srcWalletAddrsDetector diff th

-- getTxFeeMulti
--      :: MonadFees ctx m
--      => ProtocolMagic
--      -> AccountId
--      -> CId Addr
--      -> Currency
--      -> Coin
--      -> InputSelectionPolicy
--      -> m CCoin
-- getTxFeeMulti pm srcAccount dstAccount currency coin policy = do
--     ws <- askWalletSnapshot
--     let pendingAddrs = getPendingAddresses ws policy
--     utxo <- getMoneySourceUtxo ws (AccountMoneySource srcAccount)
--     outputs <- coinDistrToOutputs $ one (dstAccount, sealCurrency, coin)
--     TxFee fee <- rewrapTxError "Cannot compute transaction fee" $
--         eitherToThrow =<< runTxCreator policy (computeTxFee pm pendingAddrs utxo outputs)
--     pure $ encodeCType fee