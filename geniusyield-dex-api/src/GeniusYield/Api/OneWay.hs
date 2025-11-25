module GeniusYield.Api.OneWay
  ( oneWayAddress
  , deployScript
  )
where

import GeniusYield.TxBuilder
import GeniusYield.Types

import GeniusYield.Scripts.OneWay

-- TODO: Default GYTxOut instance

deployScript
  :: forall (v :: PlutusVersion)
   . GYScript v
  -- ^ The script to deploy.
  -> GYTxSkeleton PlutusV2 -- ???: V3 -> V2, is it safe
deployScript script =
  mustHaveOutput
    GYTxOut
      { gyTxOutAddress = oneWayAddress
      , gyTxOutValue = mempty
      , gyTxOutDatum = Just (datumFromPlutusData (), GYTxOutUseInlineDatum)
      , gyTxOutRefS = Just $ GYPlutusScript script
      }
