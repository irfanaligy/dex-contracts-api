{-# LANGUAGE TemplateHaskell #-}

module GeniusYield.OnChain.Core.Common.Staking.Stake (StakeDatum (..)) where

import GHC.Generics (Generic)
import PlutusTx qualified

import GeniusYield.OnChain.Core.Common.LedgerExports.Common

-- | The datum for staked value. Contains owner address, owner key and (optionally) the earliest time of retrieval.
data StakeDatum = StakeDatum
  { sdOwnerKey :: !PubKeyHash
  -- ^ The owner key. To retrieve the funds, the owner must sign.
  , sdOwnerAddr :: !Address
  -- ^ The owner address. This has no impact on validation.
  , sdLockedUntil :: !(Maybe POSIXTime)
  -- ^ If set, this denotes the earliest time when the funds can be retrieved.
  }
  deriving (Generic, Show)

PlutusTx.unstableMakeIsData ''StakeDatum
