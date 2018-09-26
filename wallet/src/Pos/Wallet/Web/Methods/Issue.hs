{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Muiltiple currency's transaction creation and fees

module Pos.Wallet.Web.Methods.Issue
       ( issueGold
       , issueDollar
       , destroyDollar
       ) where

import           Universum

import           Control.Monad.Except (runExcept)
import qualified Data.Map as M
import           Data.Time.Units (Second)
import           Servant.Server (err403, err405, errReasonPhrase)
import           System.Wlog (logDebug)

import           Pos.Client.KeyStorage (getSecretKeys)
import           Pos.Client.Txp.History (TxHistoryEntry (..))
import           Pos.Client.Txp.NetworkIssue (prepareGoldIssue, prepareDollarIssue, prepareDollarDestroy)
import           Pos.Client.Txp.Util (InputSelectionPolicy (..), PendingAddresses)
import           Pos.Configuration (walletTxCreationDisabled)
import           Pos.Core (Address, Coin, TxAux (..), TxOut (..), getCurrentTimestamp)
import           Pos.Core.Txp (_txOutputs)
import           Pos.Crypto (PassPhrase, ProtocolMagic, SafeSigner, ShouldCheckPassphrase (..),
                             checkPassMatches, hash, withSafeSignerUnsafe)
import           Pos.Wallet.Aeson.ClientTypes ()
import           Pos.Wallet.Aeson.WalletBackup ()
import           Pos.Wallet.Web.Account (getSKByAddressPure, getSKById)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), CTx (..), GoldIssue (..), DollarIssue (..), DollarDestroy (..))
import           Pos.Wallet.Web.Error (WalletError (..))
import           Pos.Wallet.Web.Methods.Payment (MoneySource (..), notFasterThan, getSomeMoneySourceAccount,
                                                 getMoneySourceWallet, getMoneySourceAddresses)
import           Pos.Wallet.Web.Methods.History (addHistoryTxMeta, constructCTx,
                                                 getCurChainDifficulty)
import           Pos.Wallet.Web.Methods.Txp (MonadWalletTxFull, getPendingAddresses, submitAndSaveNewPtx, rewrapTxError)
import           Pos.Wallet.Web.Pending (mkPendingTx)
import           Pos.Wallet.Web.State (AddressLookupMode (Ever), HasWAddressMeta (..), 
                                       askWalletDB, getWalletSnapshot, isWalletRestoring)
import           Pos.Wallet.Web.Util (getWalletAddrsDetector, decodeCTypeOrFail)


issueGold
    :: (MonadWalletTxFull ctx m)
    => ProtocolMagic
    -> (TxAux -> m Bool)
    -> PassPhrase
    -> AccountId
    -> GoldIssue
    -> m CTx
issueGold pm submitTx passphrase srcAccount goldIssue = do
    notFasterThan (6 :: Second) $ do
        issuedGold <- decodeCTypeOrFail $ giIssuedGold goldIssue
        let reason = giReason goldIssue
        doIssueGold
            pm
            submitTx
            passphrase
            (AccountMoneySource srcAccount)
            issuedGold
            reason
            OptimizeForHighThroughput

doIssueGold
    :: (MonadWalletTxFull ctx m)
    => ProtocolMagic
    -> (TxAux -> m Bool)
    -> PassPhrase
    -> MoneySource
    -> Coin
    -> Text
    -> InputSelectionPolicy
    -> m CTx
doIssueGold pm submitTx passphrase moneySource issuedGold reason policy = do
    logDebug "doIssueGold: pre check"
    (account, srcAddrs, pendingAddrs, getSigner) <- checkAndGet passphrase moneySource policy

    logDebug "doIssueGold: send transaction"
    th <- rewrapTxError "Cannot send transaction" $ do
        (txAux, inpTxOuts) <- prepareGoldIssue pm getSigner pendingAddrs policy srcAddrs issuedGold reason (account, passphrase)
        th <- mkHistory pm submitTx moneySource txAux inpTxOuts
        return th

    logDebug "doIssueGold: construct response"
    ctx <- mkResponse moneySource th
    return ctx

issueDollar
    :: (MonadWalletTxFull ctx m)
    => ProtocolMagic
    -> (TxAux -> m Bool)
    -> PassPhrase
    -> AccountId
    -> DollarIssue
    -> m CTx
issueDollar pm submitTx passphrase srcAccount dollarIssue = do
    notFasterThan (6 :: Second) $ do
        issuedDollar <- decodeCTypeOrFail $ diIssuedDollar dollarIssue
        lockedGold <- decodeCTypeOrFail $ diLockedGold dollarIssue
        let reason = diReason dollarIssue
        doIssueDollar
            pm
            submitTx
            passphrase
            (AccountMoneySource srcAccount)
            issuedDollar
            lockedGold
            reason
            OptimizeForHighThroughput

doIssueDollar
    :: (MonadWalletTxFull ctx m)
    => ProtocolMagic
    -> (TxAux -> m Bool)
    -> PassPhrase
    -> MoneySource
    -> Coin
    -> Coin
    -> Text
    -> InputSelectionPolicy
    -> m CTx
doIssueDollar pm submitTx passphrase moneySource issuedDollar lockedGold reason policy = do
    logDebug "doIssueDollar: pre check"
    (account, srcAddrs, pendingAddrs, getSigner) <- checkAndGet passphrase moneySource policy

    logDebug "doIssueDollar: send transaction"
    th <- rewrapTxError "Cannot send transaction" $ do
        (txAux, inpTxOuts) <- prepareDollarIssue pm getSigner pendingAddrs policy srcAddrs issuedDollar lockedGold reason (account, passphrase)
        th <- mkHistory pm submitTx moneySource txAux inpTxOuts
        return th

    logDebug "doIssueDollar: construct response"
    ctx <- mkResponse moneySource th
    return ctx

destroyDollar
    :: (MonadWalletTxFull ctx m)
    => ProtocolMagic
    -> (TxAux -> m Bool)
    -> PassPhrase
    -> AccountId
    -> DollarDestroy
    -> m CTx
destroyDollar pm submitTx passphrase srcAccount dollarDestroy = do
    notFasterThan (6 :: Second) $ do
        destroyedDollar <- decodeCTypeOrFail $ ddDestroyedDollar dollarDestroy
        unlockedGold <- decodeCTypeOrFail $ ddUnlockedGold dollarDestroy
        let reason = ddReason dollarDestroy
        doDestroyDollar
            pm
            submitTx
            passphrase
            (AccountMoneySource srcAccount)
            destroyedDollar
            unlockedGold
            reason
            OptimizeForHighThroughput

doDestroyDollar
    :: (MonadWalletTxFull ctx m)
    => ProtocolMagic
    -> (TxAux -> m Bool)
    -> PassPhrase
    -> MoneySource
    -> Coin
    -> Coin
    -> Text
    -> InputSelectionPolicy
    -> m CTx
doDestroyDollar pm submitTx passphrase moneySource destroyedDollar unlockedGold reason policy = do
    logDebug "doDestroyDollar: pre check"
    (account, srcAddrs, pendingAddrs, getSigner) <- checkAndGet passphrase moneySource policy

    logDebug "doDestroyDollar: send transaction"
    th <- rewrapTxError "Cannot send transaction" $ do
        (txAux, inpTxOuts) <- prepareDollarDestroy pm getSigner pendingAddrs policy srcAddrs destroyedDollar unlockedGold reason (account, passphrase)
        th <- mkHistory pm submitTx moneySource txAux inpTxOuts
        return th

    logDebug "doDestroyDollar: construct response"
    ctx <- mkResponse moneySource th
    return ctx

checkAndGet
    :: (MonadWalletTxFull ctx m)
    => PassPhrase
    -> MoneySource
    -> InputSelectionPolicy
    -> m (AccountId, NonEmpty Address, PendingAddresses, Address -> Maybe SafeSigner)
checkAndGet passphrase moneySource policy = do
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
    relatedAccount <- getSomeMoneySourceAccount ws moneySource
    let pendingAddrs = getPendingAddresses ws policy

    let metasAndAddresses = M.fromList $ zip (toList srcAddrs) (toList addrMetas)
    allSecrets <- getSecretKeys
    let
        getSigner :: Address -> Maybe SafeSigner
        getSigner addr = do
          addrMeta <- M.lookup addr metasAndAddresses
          sk <- rightToMaybe . runExcept $
              getSKByAddressPure allSecrets (ShouldCheckPassphrase False) passphrase addrMeta
          withSafeSignerUnsafe sk (pure passphrase) pure

    pure (relatedAccount, srcAddrs, pendingAddrs, getSigner)

mkHistory
    :: (MonadWalletTxFull ctx m)
    => ProtocolMagic
    -> (TxAux -> m Bool)
    -> MoneySource
    -> TxAux
    -> NonEmpty TxOut
    -> m TxHistoryEntry
mkHistory pm submitTx moneySource txAux inpTxOuts = do
    db <- askWalletDB
    ws <- getWalletSnapshot db
    let srcWallet = getMoneySourceWallet moneySource

    ts <- Just <$> getCurrentTimestamp
    let tx = taTx txAux
        txHash = hash tx
        inpTxOuts' = toList inpTxOuts
        dstAddrs  = map txOutAddress . toList $ _txOutputs tx
        th = THEntry txHash tx Nothing inpTxOuts' dstAddrs ts

    ptx <- mkPendingTx ws srcWallet txHash txAux th
    th <$ submitAndSaveNewPtx pm db submitTx ptx

mkResponse
    :: (MonadWalletTxFull ctx m)
    => MoneySource
    -> TxHistoryEntry
    -> m CTx
mkResponse moneySource th = do
    db <- askWalletDB
    let srcWallet = getMoneySourceWallet moneySource
    -- We add TxHistoryEntry's meta created by us in advance
    -- to make TxHistoryEntry in CTx consistent with entry in history.
    _ <- addHistoryTxMeta db srcWallet th
    diff <- getCurChainDifficulty
    ws' <- getWalletSnapshot db
    let srcWalletAddrsDetector = getWalletAddrsDetector ws' Ever srcWallet

    ctx <- fst <$> constructCTx ws' srcWallet srcWalletAddrsDetector diff th
    return ctx
