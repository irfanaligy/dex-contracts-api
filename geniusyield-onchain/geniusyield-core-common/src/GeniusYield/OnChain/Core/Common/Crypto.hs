{-# LANGUAGE TemplateHaskell #-}

module GeniusYield.OnChain.Core.Common.Crypto
  ( SignedMessage (..)
  , Signature
  , PubKey
  , PaymentPubKey
  , ScriptHash
  )
where

import GHC.Generics
import PlutusTx qualified

import GeniusYield.OnChain.Core.Common.LedgerExports.Common

type Signature = BuiltinByteString

type PubKey = LedgerBytes

type PaymentPubKey = PubKey

data SignedMessage a = SignedMessage
  { smSignature :: Signature
  -- ^ The cryptographic signature.
  , smMessageHash :: DatumHash
  -- ^ The hash of the message.
  }
  deriving (Generic, Show)

PlutusTx.unstableMakeIsData ''SignedMessage
