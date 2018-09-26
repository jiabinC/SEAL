{-# LANGUAGE PatternSynonyms      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | 'Arbitrary' unsafe instances for some types from Txp types

module Test.Pos.Txp.Arbitrary.Unsafe () where

import           Universum

import           Pos.Core.Txp (TxOut (..), pattern SealTxOut)

import           Test.Pos.Core.Arbitrary.Unsafe ()
import           Test.Pos.Util.QuickCheck.Arbitrary (ArbitraryUnsafe (..))

instance ArbitraryUnsafe TxOut where
    arbitraryUnsafe = SealTxOut <$> arbitraryUnsafe <*> arbitraryUnsafe
