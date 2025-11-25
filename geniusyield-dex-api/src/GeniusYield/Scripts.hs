module GeniusYield.Scripts
  ( GYCompiledScripts (..)
  , GYCompiledScriptsRaw
  , mkCompiledScripts
  , mkGYScripts
  -- , sptPolicyId
  -- , sptSymbol
  -- , spt
  -- , stakeAddress
  , nftMintingPolicyId
  , readCompiledScripts
  , validatorFromPly
  , mintingPolicyFromPly
  )
where

-- import GeniusYield.TxBuilder.Class
import GeniusYield.Types
-- import PlutusLedgerApi.V1 (CurrencySymbol)

import GeniusYield.Scripts.DEX (POCVersion)
import GeniusYield.Scripts.DEX qualified as DEX
import GeniusYield.Scripts.Internal
-- import GeniusYield.Scripts.SignedMint qualified as SM
-- import GeniusYield.Scripts.Staking qualified as Staking
-- import GeniusYield.Scripts.TokenSale
-- import GeniusYield.Scripts.TokenSale qualified as TS

-- should we put Aiken scripts here as well???
data GYCompiledScripts = GYCompiledScripts
  { -- tsOrderValidator :: !(TS.TokenSaleParams -> GYScript PlutusV2)
  -- , tsSptPolicy :: !(GYAddress -> TS.TokenSaleParams -> GYScript PlutusV2)
    dexPartialOrderValidator :: !(POCVersion -> GYAssetClass -> GYScript PlutusV2)
  , dexPartialOrderNftPolicy :: !(POCVersion -> GYAssetClass -> GYScript PlutusV2)
  , dexPartialOrderConfigValidator :: !(POCVersion -> GYAssetClass -> GYScript PlutusV2)
  , dexNftPolicy :: !(GYScript PlutusV2)
  , dexTwoWayOrderConfigValidator :: !(GYAssetClass -> GYScript PlutusV2)
  -- , stakingStakeValidator :: !(GYScript PlutusV1)
  -- , signedMintPolicy :: !(GYPubKeyHash -> GYScript PlutusV2)
  }

-- | 'GYScripts' constructor
mkGYScripts
  :: GYCompiledScripts
  -> DEX.GYScripts
mkGYScripts
  GYCompiledScripts
    { dexPartialOrderValidator
    , dexPartialOrderNftPolicy
    } =
    DEX.GYScripts
      { gyPartialOrderValidator = dexPartialOrderValidator
      , gyPartialOrderNFTPolicy = dexPartialOrderNftPolicy
      , gyPartialOrderNFTPolicyId = \pocVersion -> mintingPolicyId . dexPartialOrderNftPolicy pocVersion
      }

instance Show GYCompiledScripts where
  showsPrec _ _ = showString "<GYCompiledScripts>"

readCompiledScripts :: IO GYCompiledScripts
readCompiledScripts = mkCompiledScripts <$> readCompiledScriptsRaw

-- | Create a record of simple functions from a record of typed scripts.
mkCompiledScripts :: GYCompiledScriptsRaw -> GYCompiledScripts
mkCompiledScripts gycs =
  GYCompiledScripts
    { -- tsOrderValidator = TS.orderValidator gycs
    -- , tsSptPolicy = TS.sptPolicy gycs
      dexPartialOrderValidator = DEX.partialOrderValidator gycs
    , dexPartialOrderNftPolicy = DEX.partialOrderNftMintingPolicy gycs
    , dexPartialOrderConfigValidator = DEX.partialOrderConfigValidator gycs
    , dexNftPolicy = DEX.nftMintingPolicy gycs
    , dexTwoWayOrderConfigValidator = DEX.twoWayOrderConfigValidator gycs
    -- , stakingStakeValidator = Staking.stakeValidator gycs
    -- , signedMintPolicy = SM.signedMintPolicy gycs
    }

{-
sptPolicyId :: GYTxQueryMonad m => GYCompiledScripts -> TokenSaleParams -> m GYMintingPolicyId
sptPolicyId GYCompiledScripts {tsOrderValidator, tsSptPolicy} p = do
  addr <- scriptAddress $ tsOrderValidator p
  pure . mintingPolicyId $ tsSptPolicy addr p

sptSymbol :: GYTxQueryMonad m => GYCompiledScripts -> TokenSaleParams -> m CurrencySymbol
sptSymbol GYCompiledScripts {tsOrderValidator, tsSptPolicy} p = do
  addr <- scriptAddress $ tsOrderValidator p
  pure . mintingPolicyCurrencySymbol $ tsSptPolicy addr p

spt :: GYTxQueryMonad m => GYCompiledScripts -> TokenSaleParams -> m GYAssetClass
spt gycs p = do
  pid <- sptPolicyId gycs p
  return $ GYToken pid sptTokenName

stakeAddress :: GYTxQueryMonad m => GYCompiledScripts -> m GYAddress
stakeAddress GYCompiledScripts {stakingStakeValidator} = scriptAddress stakingStakeValidator
-}

nftMintingPolicyId :: GYCompiledScripts -> GYMintingPolicyId
nftMintingPolicyId GYCompiledScripts {dexNftPolicy} = mintingPolicyId dexNftPolicy
