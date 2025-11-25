{-# OPTIONS_GHC -Wno-orphans #-}

module GeniusYield.Scripts.MerkleRewards
  ( rewardsPolicy
  , rewardsValidator

    -- * Datum
  , LastRewardsAction (..)
  , RewardsDatum (..)

    -- * Redeemer
  , RewardsAction (..)
  )
where

import Control.Monad.Reader.Class (asks)
import GeniusYield.Imports
import GeniusYield.Types

import GeniusYield.Api.Types (GYApiQueryMonad)
import GeniusYield.OnChain.MerkleRewards
  ( LastRewardsAction (..)
  , RewardsAction (..)
  , RewardsDatum (..)
  )
import GeniusYield.OnChain.MerkleRewards.Compiled (originalRewardsValidator)
import GeniusYield.Scripts (GYCompiledScripts (dexNftPolicy))
import GeniusYield.Scripts.MerkleRewards.MerkleTree (zeroHash)

deriving instance Show LastRewardsAction

deriving instance Eq LastRewardsAction

deriving instance Ord LastRewardsAction

deriving instance Generic LastRewardsAction

deriving instance Show RewardsDatum

deriving instance Eq RewardsDatum

deriving instance Ord RewardsDatum

deriving instance Generic RewardsDatum

deriving instance Show RewardsAction

deriving instance Eq RewardsAction

deriving instance Generic RewardsAction

rewardsPolicy :: GYApiQueryMonad m => m (GYScript PlutusV2)
rewardsPolicy = asks dexNftPolicy

rewardsValidator :: GYApiQueryMonad m => GYPubKeyHash -> m (GYScript PlutusV2)
rewardsValidator pkh = do
  cs <- mintingPolicyIdToCurrencySymbol . mintingPolicyId <$> rewardsPolicy
  pure $ validatorFromPlutus $ originalRewardsValidator (pubKeyHashToPlutus pkh) zeroHash cs
