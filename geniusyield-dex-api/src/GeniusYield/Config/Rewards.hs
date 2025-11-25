module GeniusYield.Config.Rewards
  ( GYRewardsConfig (..)
  , GYRewardsFeeParams (..)
  , rewardsConfigIO
  )
where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text.IO qualified as TextIO (readFile)
import GHC.Generics (Generic)
import GeniusYield.Types (GYAddressBech32, GYNatural)

import GeniusYield.Api.Cache (RewardsCache)
import GeniusYield.Config.Utils (fillPlaceholders)

data GYRewardsConfig = GYRewardsConfig
  { rewardsOwnerSKeyPath :: !FilePath
  , rewardsCache :: !RewardsCache
  , rewardsFeeParams :: !GYRewardsFeeParams
  , rewardsWhitelistedGroups :: ![Text]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data GYRewardsFeeParams = GYRewardsFeeParams
  { rfpLovelaces :: !GYNatural
  -- ^ The amount of lovelaces to be charged as a fee.
  , rfpFeeAddress :: !GYAddressBech32
  -- ^ The address to which the fee will be sent.
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

rewardsConfigIO :: FilePath -> IO GYRewardsConfig
rewardsConfigIO file = do
  t <- TextIO.readFile file >>= fillPlaceholders
  case Aeson.eitherDecodeStrictText t of
    Left err -> throwIO $ userError err
    Right cfg -> pure cfg
