{-# LANGUAGE TemplateHaskell #-}

module GeniusYield.OnChain.Core.Common.TokenSale
  ( minTokenSaleDeposit
  , TokenSaleParams (..)
  , OrderDatum (..)
  , OrderAction (..)
  )
where

import GHC.Generics (Generic)
import PlutusTx qualified
import PlutusTx.Prelude qualified as PlutusTx

import GeniusYield.OnChain.Core.Common.LedgerExports.Common

{- | The minimum deposit (in lovelace) to place with an order.
This deposit will be ignored in price- and fee-calculations and will be given back to the owner in the end.
Its purpose is to make sure Cardano ledger requirements for minimal UTxO's are met.
-}
minTokenSaleDeposit :: Num a => a
minTokenSaleDeposit = 2_000_000

-- | Parametereizes a token sale.
data TokenSaleParams = TokenSaleParams
  { tspBeginSale :: !POSIXTime
  -- ^ Start time for buying the token.
  , tspEndSale :: !POSIXTime
  -- ^ Deadline for buying the token.
  , tspEndDistribution :: !POSIXTime
  -- ^ Deadline for distributing the bought tokens.
  , tspToken :: !AssetClass
  -- ^ The token on sale.
  , tspPrice :: !PlutusTx.Rational
  -- ^ Price for one token in lovelace.
  , tspSellerKey :: !PubKeyHash
  -- ^ The token seller.
  , tspMinAllocation :: !PlutusTx.Integer
  -- ^ The minimal token allocation.
  , tspFee :: !PlutusTx.Rational
  -- ^ The fees for the platform provider.
  , tspFeeAddress :: !Address
  -- ^ The address where the fees must be sent to
  }
  deriving Generic

PlutusTx.unstableMakeIsData ''TokenSaleParams

--
----------------------------------------------------------------------

{- | Datum specifying an order, given by the "owner" (who will reclaim the order)
    and the address to send the tokens to.
-}
data OrderDatum = OrderDatum
  { odOwnerKey :: !PubKeyHash
  , odOwnerAddr :: !Address
  }
  deriving (Generic, Show)

PlutusTx.unstableMakeIsData ''OrderDatum

--
----------------------------------------------------------------------

{- | Possible ways to "unlock" an order: It can either be /filled/ by the seller, sending tokens to the owner during the distribution phase,
or /cancelled/ by the owner after the distribution phase.
-}
data OrderAction
  = -- | Cancel the order.
    Cancel
  | -- | Fill the order.
    Fill
  deriving Show

PlutusTx.makeIsDataIndexed ''OrderAction [('Cancel, 0), ('Fill, 1)]
