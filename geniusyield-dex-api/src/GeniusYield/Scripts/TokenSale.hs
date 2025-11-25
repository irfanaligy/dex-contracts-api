{-# OPTIONS_GHC -Wno-orphans #-}

module GeniusYield.Scripts.TokenSale
  ( TokenSaleParams (..)
  , minTokenSaleDeposit
  , orderValidator
  , sptPolicy
  , sptTokenName

    -- * Datum
  , OrderDatum (..)

    -- * Redeemer
  , OrderAction (..)
  )
where

import GeniusYield.OnChain.Core.Common.TokenSale
import GeniusYield.Types
import PlutusLedgerApi.V1.Value
import Ply

import GeniusYield.Scripts.Internal

orderValidator :: GYCompiledScriptsRaw -> TokenSaleParams -> GYScript PlutusV2
orderValidator
  GYCompiledScriptsRaw {gycsTokenSaleOrder}
  TokenSaleParams
    { tspEndSale
    , tspEndDistribution
    , tspToken
    , tspPrice
    , tspSellerKey
    , tspMinAllocation
    , tspFee
    , tspFeeAddress
    } =
    validatorFromPly
      $ gycsTokenSaleOrder
        # tspEndSale
        # tspEndDistribution
        # cs
        # tn
        # tspPrice
        # tspSellerKey
        # tspMinAllocation
        # tspFee
        # tspFeeAddress
        # tokenNameToPlutus sptTokenName
    where
      AssetClass (cs, tn) = tspToken

sptPolicy :: GYCompiledScriptsRaw -> GYAddress -> TokenSaleParams -> GYScript PlutusV2
sptPolicy GYCompiledScriptsRaw {gycsSalePhaseToken} orderValidatorAddr TokenSaleParams {tspBeginSale, tspEndSale} = do
  mintingPolicyFromPly
    $ gycsSalePhaseToken
      # tokenNameToPlutus sptTokenName
      # tspBeginSale
      # tspEndSale
      # addressToPlutus orderValidatorAddr

sptTokenName :: GYTokenName
sptTokenName = "GY"
