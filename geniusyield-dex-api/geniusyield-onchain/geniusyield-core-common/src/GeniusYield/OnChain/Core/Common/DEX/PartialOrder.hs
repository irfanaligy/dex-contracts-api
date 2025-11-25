{-# LANGUAGE TemplateHaskell #-}

module GeniusYield.OnChain.Core.Common.DEX.PartialOrder (PartialOrderFeeOutput (..), PartialOrderContainedFee (..), PartialOrderDatum (..), PartialOrderAction (..)) where

import GHC.Generics (Generic)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as PlutusTx
import PlutusTx.Prelude qualified as PlutusTx

import GeniusYield.OnChain.Core.Common.LedgerExports.Common

-- | Representation of total fees contained in the order.
data PartialOrderContainedFee = PartialOrderContainedFee
  { pocfLovelaces :: Integer
  -- ^ Fees explicitly charged in lovelaces, like flat lovelace fee collected from maker and taker(s).
  , pocfOfferedTokens :: Integer
  -- ^ Fees explicitly collected as percentage of offered tokens from maker.
  , pocfAskedTokens :: Integer
  -- ^ Fees explicitly collected as percentage of asked tokens from taker.
  }
  deriving (Generic, Show)

PlutusTx.unstableMakeIsData ''PartialOrderContainedFee

instance Semigroup PartialOrderContainedFee where
  (<>) a b =
    PartialOrderContainedFee
      { pocfLovelaces = pocfLovelaces a + pocfLovelaces b
      , pocfOfferedTokens = pocfOfferedTokens a + pocfOfferedTokens b
      , pocfAskedTokens = pocfAskedTokens a + pocfAskedTokens b
      }

instance Monoid PartialOrderContainedFee where mempty = PartialOrderContainedFee 0 0 0

-- | Datum of the fee output.
data PartialOrderFeeOutput = PartialOrderFeeOutput
  { pofdMentionedFees :: PlutusTx.Map TxOutRef Value
  -- ^ Map, mapping order being consumed to the collected fees.
  , pofdReservedValue :: Value
  -- ^ Value reserved in this UTxO which is not to be considered as fees.
  , pofdSpentUTxORef :: Maybe TxOutRef
  -- ^ If not @Nothing@, it mentions the UTxO being consumed, whose value is used to provide for UTxOs minimum ada requirement.
  }
  deriving (Generic, Show)

PlutusTx.unstableMakeIsData ''PartialOrderFeeOutput

-- | Datum specifying a partial order.
data PartialOrderDatum = PartialOrderDatum
  { podOwnerKey :: PubKeyHash
  -- ^ Public key hash of the owner. Order cancellations must be signed by this.
  , podOwnerAddr :: Address
  -- ^ Address of the owner. Payments must be made to this address.
  , podOfferedAsset :: AssetClass
  -- ^ The asset being offered.
  , podOfferedOriginalAmount :: Integer
  -- ^ Original number of units being offered. Initially, this would be same as `podOfferedAmount`.
  , podOfferedAmount :: Integer
  -- ^ The number of units being offered.
  , podAskedAsset :: AssetClass
  -- ^ The asset being asked for as payment.
  , podPrice :: PlutusTx.Rational
  -- ^ The price for one unit of the offered asset.
  , podNFT :: TokenName
  -- ^ Token name of the NFT identifying this order.
  , podStart :: Maybe POSIXTime
  -- ^ The time when the order can earliest be filled (optional).
  , podEnd :: Maybe POSIXTime
  -- ^ The time when the order can latest be filled (optional).
  , podPartialFills :: Integer
  -- ^ Number of partial fills order has undergone, initially would be 0.
  , podMakerLovelaceFlatFee :: Integer
  -- ^ Flat fee (in lovelace) paid by the maker.
  , podTakerLovelaceFlatFee :: Integer
  -- ^ Flat fee (in lovelace) paid by the taker.
  , podContainedFee :: PartialOrderContainedFee
  -- ^ Total fees contained in the order.
  , podContainedPayment :: Integer
  -- ^ Payment (in asked asset) contained in the order.
  }
  deriving (Generic, Show)

PlutusTx.unstableMakeIsData ''PartialOrderDatum

data PartialOrderAction
  = PartialCancel
  | PartialFill Integer
  | CompleteFill
  deriving (Generic, Show)

PlutusTx.makeIsDataIndexed ''PartialOrderAction [('PartialCancel, 0), ('PartialFill, 1), ('CompleteFill, 2)]
