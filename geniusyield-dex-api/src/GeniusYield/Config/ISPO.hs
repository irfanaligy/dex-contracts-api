module GeniusYield.Config.ISPO
  ( GYIspoConfig (..)
  , GYIspoDistribution (..)
  , gyIspoConfigIO
  , gyIspoDistributionIO
  )
where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Csv qualified as Csv
import GeniusYield.Imports
import GeniusYield.Types

data GYIspoConfig = GYIspoConfig
  { ispDistFilePath :: !FilePath
  -- ^ File path to store calculated Distribution
  , ispTxProgressFilePath :: !FilePath
  -- ^ File path to store distribution Hash
  , ispDistKeyPath :: !FilePath
  -- ^ Path to Distributor Signing key
  , ispDistAddr :: ![GYAddressBech32]
  -- ^ Address of Distributor
  , ispCollateralRef :: !GYTxOutRef
  -- ^ Collateral of Distributor
  , ispDistToken :: !GYAssetClass
  -- ^ Token to distribute
  }
  deriving (Aeson.FromJSON, Aeson.ToJSON, Generic, Show)

data GYIspoDistribution = GYIspoDistribution
  { ispDistStakeKey :: !String
  -- ^ Stake key of delegator
  , ispDistPayAddr :: !GYAddress
  -- ^ Payment address for sending token to delegator
  , ispDistAmount :: !Natural
  -- ^ Amount of Token to distribute
  }
  deriving (Aeson.ToJSON, Csv.FromRecord, Csv.ToRecord, Generic, Show)

gyIspoConfigIO :: FilePath -> IO GYIspoConfig
gyIspoConfigIO file = do
  bs <- LBS.readFile file
  case Aeson.eitherDecode' bs of
    Left err -> fail $ show err
    Right rslt -> pure rslt

gyIspoDistributionIO :: FilePath -> IO [GYIspoDistribution]
gyIspoDistributionIO path = do
  e <- fmap toList . Csv.decode Csv.NoHeader <$> LBS.readFile path
  case e of
    Left err -> fail $ show err
    Right rslt -> pure rslt
