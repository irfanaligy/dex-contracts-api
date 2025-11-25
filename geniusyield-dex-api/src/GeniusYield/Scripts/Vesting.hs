{-# OPTIONS_GHC -Wno-orphans #-}

module GeniusYield.Scripts.Vesting
  ( vestingValidator
  , minDeposit

    -- * Datum
  , VestingDatum (..)

    -- * Redeemer
  , VestingAction (..)

    -- * For Testing
  , originalVestingValidator
  )
where

import GeniusYield.Imports
import GeniusYield.Types

import GeniusYield.OnChain.Vesting (VestingAction (..), VestingDatum (..))
import GeniusYield.OnChain.Vesting qualified as OnChain
import GeniusYield.OnChain.Vesting.Compiled (originalVestingValidator)

deriving instance Show VestingDatum

deriving instance Generic VestingDatum

deriving instance Show VestingAction

deriving instance Generic VestingAction

vestingValidator :: GYScript PlutusV2
vestingValidator =
  validatorFromPlutus originalVestingValidator

minDeposit :: GYValue
minDeposit = either (error . show) id $ valueFromPlutus OnChain.minDeposit
