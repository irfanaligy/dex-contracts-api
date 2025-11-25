{-# OPTIONS_GHC -Wno-orphans #-}

module GeniusYield.Scripts.Staking.Stake
  ( -- * Validator
    stakeValidator

    -- * Datum
  , StakeDatum (..)
  )
where

import GeniusYield.OnChain.Core.Common.Staking.Stake
import GeniusYield.Types

import GeniusYield.Scripts.Internal

stakeValidator :: GYCompiledScriptsRaw -> GYScript PlutusV1
stakeValidator GYCompiledScriptsRaw {gycsStakingStake} = validatorFromPly gycsStakingStake
