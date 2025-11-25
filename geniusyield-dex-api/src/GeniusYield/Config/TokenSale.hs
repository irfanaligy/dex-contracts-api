module GeniusYield.Config.TokenSale
  ( GYTokenSaleConfig (..)
  , tsConfigIO
  )
where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import GeniusYield.Imports
import GeniusYield.Types (GYAddressBech32, GYTxOutRef)

import GeniusYield.Api.TokenSale (TokenSaleParams)

data GYTokenSaleConfig = GYTokenSaleConfig
  { tsParams :: !TokenSaleParams
  , tsSupply :: !Natural
  , tsMaxAlloc :: !Natural
  , tsSKeyFile :: !FilePath
  , tsWhitelist :: !FilePath
  , tsOrdersFile :: !FilePath
  , tsSummaryFile :: !FilePath
  , tsDistFile :: !FilePath
  , tsCollateralRef :: !GYTxOutRef
  , tsSellerAddr :: ![GYAddressBech32]
  , tsPaymentAddr :: !(Maybe GYAddressBech32)
  }
  deriving (Generic, Show)

instance FromJSON GYTokenSaleConfig

tsConfigIO :: FilePath -> IO GYTokenSaleConfig
tsConfigIO file = do
  bs <- LBS.readFile file
  case Aeson.eitherDecode' bs of
    Left err -> throwIO $ userError err
    Right cfg -> return cfg
