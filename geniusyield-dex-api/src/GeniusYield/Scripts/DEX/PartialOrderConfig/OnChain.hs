{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module GeniusYield.Scripts.DEX.PartialOrderConfig.OnChain (
  PartialOrderConfigDatum (..),
) where

import GHC.Generics (Generic)
import PlutusLedgerApi.V1 (Address, PubKeyHash)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..))
import PlutusTx qualified
import PlutusTx.Prelude (Integer)
import PlutusTx.Ratio (Rational)
import Prelude qualified as P

data PartialOrderConfigDatum = PartialOrderConfigDatum
  { pocdSignatories :: [PubKeyHash]
  -- ^ Public key hashes of the potential signatories.
  , pocdReqSignatories :: Integer
  -- ^ Number of required signatures.
  , pocdNftSymbol :: CurrencySymbol
  -- ^ Currency symbol of the partial order Nft.
  , pocdFeeAddr :: Address
  -- ^ Address to which fees are paid.
  , pocdMakerFeeFlat :: Integer
  -- ^ Flat fee (in lovelace) paid by the maker.
  , pocdMakerFeeRatio :: Rational
  -- ^ Proportional fee (in the offered token) paid by the maker.
  , pocdTakerFee :: Integer
  -- ^ Flat fee (in lovelace) paid by the taker.
  , pocdMinDeposit :: Integer
  -- ^ Minimum required deposit (in lovelace).
  }
  deriving (Generic, P.Show)

PlutusTx.unstableMakeIsData ''PartialOrderConfigDatum
