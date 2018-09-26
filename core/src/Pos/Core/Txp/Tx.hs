module Pos.Core.Txp.Tx
       ( Tx (..)
       , checkTx
       , txInputs
       , txOutputs
       , txAttributes
       , txF

       , TxId
       , TxAttributes

       , TxIn (..)
       , isTxInUnknown

       , UserCert (..)
       , SealCert (..)
       , SealState (..)
       , TxOut (..)
       , _TxOut
       , pattern SealTxOut
       , matchCurrency
       , isPlainTxOut
       , isCertTxOut
       , isSealStateTxOut
       , safeGetTxOutValue
       , isGoldCoinStateTxOut
       , isGoldDollarStateTxOut
       ) where

import           Universum

import           Control.Lens (makeLenses, makePrisms)
import           Control.Monad.Except (MonadError (throwError))
import qualified Data.Text.Buildable as Buildable
import           Formatting (Format, bprint, build, builder, int, sformat, (%), stext)
import           Serokell.Util.Base16 (base16F)
import           Serokell.Util.Text (listJson)
import           Serokell.Util.Verify (VerificationRes (..), verResSingleF, verifyGeneric)

import           Pos.Binary.Class (Bi)
import           Pos.Core.Common (Address (..), Coin (..), Currency, pattern SealCoin, mkCoin, checkCoin, coinF, checkCurrency, currencyF)
import           Pos.Crypto (Hash, hash, shortHashF)
import           Pos.Data.Attributes (Attributes, areAttributesKnown)

----------------------------------------------------------------------------
-- Tx
----------------------------------------------------------------------------

-- | Transaction.
--
-- NB: transaction witnesses are stored separately.
data Tx = UnsafeTx
    { _txInputs     :: !(NonEmpty TxIn)  -- ^ Inputs of transaction.
    , _txOutputs    :: !(NonEmpty TxOut) -- ^ Outputs of transaction.
    , _txAttributes :: !TxAttributes     -- ^ Attributes of transaction
    } deriving (Eq, Ord, Generic, Show, Typeable)

instance Hashable Tx

instance Bi Tx => Buildable Tx where
    build tx@(UnsafeTx{..}) =
        bprint
            ("Tx "%build%
             " with inputs "%listJson%", outputs: "%listJson % builder)
            (hash tx) _txInputs _txOutputs attrsBuilder
      where
        attrs = _txAttributes
        attrsBuilder | areAttributesKnown attrs = mempty
                     | otherwise = bprint (", attributes: "%build) attrs

instance NFData Tx

-- | Specialized formatter for 'Tx'.
txF :: Bi Tx => Format r (Tx -> r)
txF = build

-- | Verify inputs and outputs are non empty; have enough coins.
checkTx
    :: MonadError Text m
    => Tx
    -> m ()
checkTx it =
    case verRes of
        VerSuccess -> pure ()
        failure    -> throwError $ verResSingleF failure
  where
    verRes =
        verifyGeneric $
        concat $ zipWith outputPredicates [0 ..] $ toList (_txOutputs it)
    outputPredicates (i :: Word) TxOut {..} =
        [ ( txOutValue > Coin 0
          , sformat
                ("output #"%int%" has non-positive value: "%coinF)
                i txOutValue
          )
        , ( isRight (checkCoin txOutValue)
          , sformat
                ("output #"%int%" has invalid coin")
                i
          )
        , ( isRight (checkCurrency txOutCurrency)
          , sformat
                ("output #"%int%" has invalid currency")
                i
          )
        ]

    outputPredicates _ _ = []

--------------------------------------------------------------------------------
-- TxId
--------------------------------------------------------------------------------

-- | Represents transaction identifier as 'Hash' of 'Tx'.
type TxId = Hash Tx

--------------------------------------------------------------------------------
-- TxAttributes
--------------------------------------------------------------------------------

-- | Represents transaction attributes: map from 1-byte integer to
-- arbitrary-type value. To be used for extending transaction with new
-- fields via softfork.
type TxAttributes = Attributes ()

--------------------------------------------------------------------------------
-- TxIn
--------------------------------------------------------------------------------

-- | Transaction arbitrary input.
data TxIn
    = TxInUtxo
    { -- | Which transaction's output is used
      txInHash  :: !TxId
      -- | Index of the output in transaction's outputs
    , txInIndex :: !Word32
    }
    | TxInUserCert
    {
      txInHash  :: !TxId
    , txInIndex :: !Word32
    }
    | TxInSealState
    {
      txInHash  :: !TxId
    , txInIndex :: !Word32
    , txInSealCert :: !SealCert
    }
    | TxInUnknown !Word8 !ByteString
    deriving (Eq, Ord, Generic, Show, Typeable)

instance Hashable TxIn

instance Buildable TxIn where
    build TxInUtxo {..}        = bprint ("TxInUtxo "%shortHashF%" #"%int) txInHash txInIndex
    build TxInUserCert {..}    = bprint ("TxInUserCert "%shortHashF%" #"%int) txInHash txInIndex
    build TxInSealState {..}   = bprint ("TxInSealState "%shortHashF%" #"%int%" "%build) txInHash txInIndex txInSealCert
    build (TxInUnknown tag bs) = bprint ("TxInUnknown "%int%" "%base16F) tag bs

instance NFData TxIn

isTxInUnknown :: TxIn -> Bool
isTxInUnknown (TxInUnknown _ _) = True
isTxInUnknown _                 = False

--------------------------------------------------------------------------------
-- TxOut
--------------------------------------------------------------------------------

-- | Transaction output.
data TxOut 
    = TxOut
    { txOutAddress  :: !Address
    , txOutCurrency :: !Currency
    , txOutValue    :: !Coin
    } 
    | TxOutUserCert 
    { txOutAddress  :: !Address
    , txOutCert     :: !UserCert
    }
    | TxOutSealState
    { txOutAddress  :: !Address
    , txOutSealState :: !SealState
    }
    deriving (Eq, Ord, Generic, Show, Typeable)

data UserCert
    = UserCert !Text !Text
    deriving (Eq, Ord, Generic, Show, Typeable)

data SealCert
    = GoldCoinIssueCert {
        issuedCoin :: Coin
      , reason :: Text
    }
    | GoldDollarIssueCert {
        issuedCoin :: Coin
      , lockedGoldCoin :: Coin
      , reason :: Text
    }
    | GoldDollarDestroyCert {
        destroyedCoin :: Coin
      , unlockedGoldCoin :: Coin
      , reason :: Text
    }
    deriving (Eq, Ord, Generic, Show, Typeable)
    
data SealState
    = GoldCoinState {
        totalCoin :: Coin
    }
    | GoldDollarState {
        totalCoin :: Coin
      , totalLockedGoldCoin :: Coin
    }
    deriving (Eq, Ord, Generic, Show, Typeable)

instance Hashable UserCert
instance Hashable SealCert
instance Hashable SealState
instance Hashable TxOut

instance Buildable UserCert where
    build (UserCert certHash content) = bprint ("UserCert "%stext%" : "%stext) certHash content

instance Buildable SealCert where
    build (GoldCoinIssueCert{..}) = bprint ("GoldCoinIssueCert "%coinF%", reason: "%stext) issuedCoin reason
    build (GoldDollarIssueCert{..} ) = bprint ("GoldDollarIssueCert "%coinF%"GoldDollar : "%coinF%stext) issuedCoin lockedGoldCoin reason
    build (GoldDollarDestroyCert{..} ) = bprint ("GoldDollarDestroyCert "%coinF%" : "%coinF%stext) destroyedCoin unlockedGoldCoin reason

instance Buildable SealState where
    build (GoldCoinState{..}) = bprint ("GoldCoinState "%coinF) totalCoin
    build (GoldDollarState{..}) = bprint ("GoldDollarState "%coinF%" locked GoldCoin"%coinF) totalCoin totalLockedGoldCoin

instance Buildable TxOut where
    build TxOut {..} =
        bprint ("TxOut "%coinF%" in "%currencyF%" -> "%build) txOutValue txOutCurrency txOutAddress
    build TxOutUserCert {..} = bprint ("TxOutUserCert "%build%" -> "%build) txOutAddress txOutCert
    build TxOutSealState {..} = bprint ("TxOutSealState "%build%" -> "%build) txOutAddress txOutSealState

instance NFData UserCert
instance NFData SealCert
instance NFData SealState
instance NFData TxOut

makePrisms ''TxOut

pattern SealTxOut :: Address -> Coin -> TxOut
pattern SealTxOut addr value = TxOut addr SealCoin value

--------------------------------------------------------------------------------
-- Tx Lenses
--------------------------------------------------------------------------------

makeLenses ''Tx

matchCurrency ::  Currency -> TxOut -> Bool
matchCurrency currency TxOut{..} = (txOutCurrency == currency)
matchCurrency _ _ = False

isPlainTxOut ::  TxOut -> Bool
isPlainTxOut TxOut{} = True
isPlainTxOut _ = False

isCertTxOut ::  TxOut -> Bool
isCertTxOut TxOutUserCert{} = True
isCertTxOut _ = False

isSealStateTxOut :: TxOut -> Bool
isSealStateTxOut TxOutSealState{} = True
isSealStateTxOut _ = False

safeGetTxOutValue :: TxOut -> Coin
safeGetTxOutValue TxOut{..} = txOutValue
safeGetTxOutValue _ = mkCoin 0

isGoldCoinStateTxOut :: TxOut -> Bool
isGoldCoinStateTxOut TxOutSealState{txOutSealState = GoldCoinState{}} = True
isGoldCoinStateTxOut _ = False

isGoldDollarStateTxOut :: TxOut -> Bool
isGoldDollarStateTxOut TxOutSealState{txOutSealState = GoldDollarState{}} = True
isGoldDollarStateTxOut _ = False