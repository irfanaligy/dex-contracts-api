{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GeniusYield.Api.TokenSale.Types
  ( TokenSaleParams (..)
  , OrderInfo (..)
  )
where

import Control.Lens ((.~), (?~))
import Data.Aeson qualified as Aeson
import Data.Csv qualified as Csv
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal.Schema qualified as Swagger
import GeniusYield.Imports
import GeniusYield.Types

{- $setup

>>> :set -XOverloadedStrings -XTypeApplications -XNumericUnderscores
>>> import qualified Data.Aeson                 as Aeson
>>> import qualified Data.ByteString.Lazy.Char8 as LBS8
>>> import qualified Data.Csv                   as Csv
>>> import           GeniusYield.Types
-}

data TokenSaleParams = TokenSaleParams
  { tspBeginSale :: !GYTime
  -- ^ Start time for buying the token.
  , tspEndSale :: !GYTime
  -- ^ Deadline for buying the token.
  , tspEndDistribution :: !GYTime
  -- ^ Deadline for distributing the bought tokens.
  , tspToken :: !GYAssetClass
  -- ^ The token on sale.
  , tspPrice :: !GYRational
  -- ^ Price for one token in lovelace.
  , tspSellerKey :: !GYPubKeyHash
  -- ^ The token seller.
  , tspMinAllocation :: !Natural
  -- ^ The minimal token allocation.
  , tspFee :: !GYRational
  -- ^ The fee for the platform provider.
  , tspFeeAddress :: !GYAddressBech32
  -- ^ The address where the fees must be sent to.
  }
  deriving stock (Generic, Show)

{- |

>>> LBS8.putStrLn $ Aeson.encode $ TokenSaleParams "2022-10-01T00:00:00Z" "2022-10-08T00:00:00Z" "2022-10-10T00:00:00Z" "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef.474f4c44" 0.4 "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d" 50_000_000 0.01 "addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5"
{"beginSale":"2022-10-01T00:00:00Z","endSale":"2022-10-08T00:00:00Z","endDistribution":"2022-10-10T00:00:00Z","token":"ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef.474f4c44","price":"0.4","sellerKey":"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d","minAllocation":50000000,"fee":"1.0e-2","feeAddress":"addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5"}
-}
instance ToJSON TokenSaleParams where
  toJSON TokenSaleParams {..} =
    Aeson.object
      [ "beginSale" Aeson..= tspBeginSale
      , "endSale" Aeson..= tspEndSale
      , "endDistribution" Aeson..= tspEndDistribution
      , "token" Aeson..= tspToken
      , "price" Aeson..= tspPrice
      , "sellerKey" Aeson..= tspSellerKey
      , "minAllocation" Aeson..= tspMinAllocation
      , "fee" Aeson..= tspFee
      , "feeAddress" Aeson..= tspFeeAddress
      ]

  toEncoding TokenSaleParams {..} =
    Aeson.pairs
      ( "beginSale" Aeson..= tspBeginSale
          <> "endSale" Aeson..= tspEndSale
          <> "endDistribution" Aeson..= tspEndDistribution
          <> "token" Aeson..= tspToken
          <> "price" Aeson..= tspPrice
          <> "sellerKey" Aeson..= tspSellerKey
          <> "minAllocation" Aeson..= tspMinAllocation
          <> "fee" Aeson..= tspFee
          <> "feeAddress" Aeson..= tspFeeAddress
      )

instance Swagger.ToSchema TokenSaleParams where
  declareNamedSchema _ = do
    gyTimeSchema <- Swagger.declareSchemaRef @GYTime Proxy
    gyAssetClassSchema <- Swagger.declareSchemaRef @GYAssetClass Proxy
    gYRationalSchema <- Swagger.declareSchemaRef @GYRational Proxy
    gyPubKeyHasSchema <- Swagger.declareSchemaRef @GYPubKeyHash Proxy
    naturalSchema <- Swagger.declareSchemaRef @Natural Proxy
    gyAddressSchema <- Swagger.declareSchemaRef @GYAddressBech32 Proxy

    return
      $ Swagger.named "tokensale_params"
      $ mempty
        & Swagger.type_
        ?~ Swagger.SwaggerObject
          & Swagger.properties
        .~ [ ("beginSale", gyTimeSchema)
           , ("endSale", gyTimeSchema)
           , ("endDistribution", gyTimeSchema)
           , ("token", gyAssetClassSchema)
           , ("price", gYRationalSchema)
           , ("sellerKey", gyPubKeyHasSchema)
           , ("minAllocation", naturalSchema)
           , ("fee", gYRationalSchema)
           , ("feeAddress", gyAddressSchema)
           ]
          & Swagger.maxProperties
        ?~ 9
          & Swagger.minProperties
        ?~ 9

{- |

>>> Aeson.decode @TokenSaleParams "{\"fee\":\"0.01\",\"feeAddress\":\"addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5\",\"minAllocation\":50000000,\"beginSale\":\"2022-10-01T00:00:00Z\",\"endSale\":\"2022-10-08T00:00:00Z\",\"endDistribution\":\"2022-10-10T00:00:00Z\",\"token\":\"ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef.474f4c44\",\"price\":\"0.4\",\"sellerKey\":\"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d\"}"
Just (TokenSaleParams {tspBeginSale = GYTime 1664582400s, tspEndSale = GYTime 1665187200s, tspEndDistribution = GYTime 1665360000s, tspToken = GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "GOLD", tspPrice = GYRational (2 % 5), tspSellerKey = GYPubKeyHash "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d", tspMinAllocation = 50000000, tspFee = GYRational (1 % 100), tspFeeAddress = unsafeAddressFromText "addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5"})
-}
instance FromJSON TokenSaleParams where
  parseJSON = Aeson.withObject "TokenSaleParams" $ \v ->
    TokenSaleParams
      <$> v Aeson..: "beginSale"
      <*> v Aeson..: "endSale"
      <*> v Aeson..: "endDistribution"
      <*> v Aeson..: "token"
      <*> v Aeson..: "price"
      <*> v Aeson..: "sellerKey"
      <*> v Aeson..: "minAllocation"
      <*> v Aeson..: "fee"
      <*> v Aeson..: "feeAddress"

{- |

>>> Csv.encodeWith Csv.defaultEncodeOptions {Csv.encUseCrLf = False} [OrderInfo "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189#1" "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d" (unsafeAddressFromText "addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5") 100_000_000 100]
"4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189#1,e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d,addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5,100000000,100\n"

>>> Csv.decode @OrderInfo Csv.NoHeader "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189#1,e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d,addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5,100000000,100\n"
Right [OrderInfo {oiRef = GYTxOutRef (TxIn "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189" (TxIx 1)), oiOwnerKey = GYPubKeyHash "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d", oiOwnerAddr = unsafeAddressFromText "addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5", oiLovelace = 100000000, oiRequest = 100}]
-}
data OrderInfo = OrderInfo
  { oiRef :: !GYTxOutRef
  -- ^ The order UTxO.
  , oiOwnerKey :: !GYPubKeyHash
  -- ^ The order owner's payment pub key hash.
  , oiOwnerAddr :: !GYAddress
  -- ^ The order owner's address.
  , oiLovelace :: !Natural
  -- ^ The lovelace amount in the order.
  , oiRequest :: !Natural
  -- ^ The requested number of tokens.
  }
  deriving stock (Generic, Show)
  deriving anyclass (Csv.FromRecord, Csv.ToRecord, FromJSON, Swagger.ToSchema, ToJSON)
