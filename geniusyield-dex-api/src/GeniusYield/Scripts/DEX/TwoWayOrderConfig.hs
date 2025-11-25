{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module GeniusYield.Scripts.DEX.TwoWayOrderConfig
  ( twoWayOrderConfigValidator
  , TwoWayOrderConfigDatumF (..)
  , TwoWayOrderConfigDatum
  )
where

import GHC.Generics (type Generic)
import GeniusYield.Types
import PlutusLedgerApi.V2
  ( Address
  , BuiltinData
  , CurrencySymbol
  , PubKeyHash
  , ScriptHash (..)
  )
import PlutusTx
  ( FromData (fromBuiltinData)
  , ToData (toBuiltinData)
  , unstableMakeIsData
  )
import PlutusTx.Ratio qualified as P (Rational)
import Ply ((#))

import GeniusYield.Scripts.Internal

data TwoWayOrderConfigDatumF addr = TwoWayOrderConfigDatum
  { twocdSignatories :: ![GYPubKeyHash]
  -- ^ Public key hashes of the potential signatories.
  , twocdReqSignatories :: !Integer
  -- ^ Number of required signatures.
  , twocdNftSymbol :: !GYMintingPolicyId
  -- ^ Minting Policy Id of the two-way order Nft.
  , twocdFillHash :: !GYScriptHash
  -- ^ Script to fill the order.
  , twocdCancelHash :: !GYScriptHash
  -- ^ Script to cancel the order.
  , twocdFeeAddr :: !addr
  -- ^ Address to which fees are paid.
  , twocdMakerFeeFlat :: !Integer
  -- ^ Flat fee (in lovelace) paid by the maker.
  , twocdMakerFeeRatio :: !GYRational
  -- ^ Proportional fee (in the offered token) paid by the maker.
  , twocdTakerFeeFlat :: !Integer
  -- ^ Flat fee (in lovelace) paid by the taker.
  , twocdTakerFeeRatio :: !GYRational
  -- ^ Proportional fee (in the offered token) paid by the taker.
  , twocdOracleFreshnessSeconds :: !Integer
  -- ^ Maximum age (in seconds) for oracle prices accepted by fills.
  , twocdMinDeposit :: !Integer
  -- ^ Minimum required deposit (in lovelace).
  }
  deriving (Functor, Generic, Show)

type TwoWayOrderConfigDatum = TwoWayOrderConfigDatumF GYAddress

-- TODO: refactor, we don't actually need TwoWayOrderConfigDatum'
data TwoWayOrderConfigDatum' = TwoWayOrderConfigDatum'
  { twocd'Signatories :: ![PubKeyHash]
  -- ^ Public key hashes of the potential signatories.
  , twocd'ReqSignatories :: !Integer
  -- ^ Number of required signatures.
  , twocd'NftSymbol :: !CurrencySymbol
  -- ^ Minting Policy Id of the two-way order Nft.
  , twocd'FillHash :: !ScriptHash
  -- ^ Script to fill the order.
  , twocd'CancelHash :: !ScriptHash
  -- ^ Script to cancel the order.
  , twocd'FeeAddr :: !Address
  -- ^ Address to which fees are paid.
  , twocd'MakerFeeFlat :: !Integer
  -- ^ Flat fee (in lovelace) paid by the maker.
  , twocd'MakerFeeRatio :: !P.Rational
  -- ^ Proportional fee (in the offered token) paid by the maker.
  , twocd'TakerFeeFlat :: !Integer
  -- ^ Flat fee (in lovelace) paid by the taker.
  , twocd'TakerFeeRatio :: !P.Rational
  -- ^ Proportional fee (in the offered token) paid by the taker.
  , twocd'OracleFreshnessSeconds :: !Integer
  -- ^ Maximum age (in seconds) for oracle prices accepted by fills.
  , twocd'MinDeposit :: !Integer
  -- ^ Minimum required deposit (in lovelace).
  }
  deriving (Generic, Show)

unstableMakeIsData ''TwoWayOrderConfigDatum'

instance ToData (TwoWayOrderConfigDatumF Address) where
  toBuiltinData :: TwoWayOrderConfigDatumF Address -> BuiltinData
  toBuiltinData TwoWayOrderConfigDatum {..} =
    toBuiltinData
      TwoWayOrderConfigDatum'
        { twocd'Signatories = pubKeyHashToPlutus <$> twocdSignatories
        , twocd'ReqSignatories = twocdReqSignatories
        , twocd'NftSymbol = mintingPolicyIdToCurrencySymbol twocdNftSymbol
        , twocd'FillHash = scriptHashToPlutus twocdFillHash
        , twocd'CancelHash = scriptHashToPlutus twocdCancelHash
        , twocd'FeeAddr = twocdFeeAddr
        , twocd'MakerFeeFlat = twocdMakerFeeFlat
        , twocd'MakerFeeRatio = rationalToPlutus twocdMakerFeeRatio
        , twocd'TakerFeeFlat = twocdTakerFeeFlat
        , twocd'TakerFeeRatio = rationalToPlutus twocdTakerFeeRatio
        , twocd'OracleFreshnessSeconds = twocdOracleFreshnessSeconds
        , twocd'MinDeposit = twocdMinDeposit
        }

instance ToData TwoWayOrderConfigDatum where
  toBuiltinData :: TwoWayOrderConfigDatum -> BuiltinData
  toBuiltinData = toBuiltinData . fmap addressToPlutus

instance FromData (TwoWayOrderConfigDatumF Address) where
  fromBuiltinData :: BuiltinData -> Maybe (TwoWayOrderConfigDatumF Address)
  fromBuiltinData d = do
    TwoWayOrderConfigDatum' {..} <- fromBuiltinData d
    signatories <- fromEither $ mapM pubKeyHashFromPlutus twocd'Signatories
    nftSymbol <- fromEither $ mintingPolicyIdFromCurrencySymbol twocd'NftSymbol
    fillHash <- fromEither $ validatorHashFromPlutus twocd'FillHash
    cancelHash <- fromEither $ validatorHashFromPlutus twocd'CancelHash

    pure
      TwoWayOrderConfigDatum
        { twocdSignatories = signatories
        , twocdReqSignatories = twocd'ReqSignatories
        , twocdNftSymbol = nftSymbol
        , twocdFillHash = fillHash
        , twocdCancelHash = cancelHash
        , twocdFeeAddr = twocd'FeeAddr
        , twocdMakerFeeFlat = twocd'MakerFeeFlat
        , twocdMakerFeeRatio = rationalFromPlutus twocd'MakerFeeRatio
        , twocdTakerFeeFlat = twocd'TakerFeeFlat
        , twocdTakerFeeRatio = rationalFromPlutus twocd'TakerFeeRatio
        , twocdOracleFreshnessSeconds = twocd'OracleFreshnessSeconds
        , twocdMinDeposit = twocd'MinDeposit
        }
    where
      fromEither :: Either e a -> Maybe a
      fromEither = either (const Nothing) Just

twoWayOrderConfigValidator
  :: GYCompiledScriptsRaw -> GYAssetClass -> GYScript PlutusV2
twoWayOrderConfigValidator GYCompiledScriptsRaw {gycsDEXTwoWayOrderConfig} ac =
  validatorFromPly $ gycsDEXTwoWayOrderConfig # assetClassToPlutus ac
