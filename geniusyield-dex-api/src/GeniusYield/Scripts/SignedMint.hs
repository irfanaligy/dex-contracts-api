{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GeniusYield.Scripts.SignedMint
  ( signedMintPolicy
  )
where

import GeniusYield.Types (GYPubKeyHash, GYScript, PlutusVersion (PlutusV2), pubKeyHashToPlutus)
import Ply ((#))

import GeniusYield.Scripts.Internal (GYCompiledScriptsRaw (..), mintingPolicyFromPly)

signedMintPolicy :: GYCompiledScriptsRaw -> GYPubKeyHash -> GYScript PlutusV2
signedMintPolicy GYCompiledScriptsRaw {gycsSignedMint} pkh =
  mintingPolicyFromPly $ gycsSignedMint # pubKeyHashToPlutus pkh
