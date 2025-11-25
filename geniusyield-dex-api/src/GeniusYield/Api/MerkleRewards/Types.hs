module GeniusYield.Api.MerkleRewards.Types
  ( RewardsInfo (..)
  , Rewardee (..)
  , Rewards (..)
  , GYRewardsException (..)
  , RewardsStep (..)
  , Reconstructor
  )
where

import Data.Map.Strict qualified as Map
import GeniusYield.HTTP.Errors (IsGYApiError)
import GeniusYield.Imports
import GeniusYield.Types
  ( GYAddress
  , GYDatumHash
  , GYPubKeyHash
  , GYStakeKeyHash
  , GYTokenName
  , GYTxOutRef
  , GYValue
  )
import PlutusLedgerApi.V2 (PubKeyHash)

import GeniusYield.Scripts.MerkleRewards (LastRewardsAction (..))
import GeniusYield.Scripts.MerkleRewards.Hashable (Hash)
import GeniusYield.Scripts.MerkleRewards.MerkleTree (MerkleTree)

data RewardsInfo = RewardsInfo
  { riInfo :: !Text
  -- ^ Information about these rewards.
  , riRef :: !GYTxOutRef
  -- ^ The reference.
  , riAddress :: !GYAddress
  -- ^ The address.
  , riValue :: !GYValue
  -- ^ The total value of rewards.
  , riPrevious :: !(Maybe GYDatumHash)
  -- ^ The hash of the previous datum.
  , riNFT :: !GYTokenName
  -- ^ The name of the NFT identifying these Rewards.
  , riRoot :: !Hash
  -- ^ The Merkle Root.
  , riDepth :: !Natural
  -- ^ The depth of the Merkle Tree.
  , riLastAction :: !LastRewardsAction
  -- ^ The last action leading to this state.
  }
  deriving (Eq, Ord, Show)

data Rewardee = WalletRewardee !GYStakeKeyHash | BotRewardee !GYPubKeyHash
  deriving (Eq, Ord, Show)

newtype Rewards = Rewards {getRewards :: Map GYPubKeyHash GYValue}
  deriving (Eq, Ord, Show)

instance ToJSON Rewards where
  toJSON = toJSON . Map.toList . getRewards

instance FromJSON Rewards where
  parseJSON = fmap (Rewards . Map.fromList) . parseJSON

data GYRewardsException = GYInvalidClaim !(MerkleTree (PubKeyHash, GYValue)) !Rewardee
  deriving (Eq, Exception, IsGYApiError, Ord, Show)

data RewardsStep
  = RSPlaced !Rewards
  | RSWithdrawn !GYPubKeyHash !GYDatumHash
  deriving (Eq, FromJSON, Generic, Ord, Show, ToJSON)

type Reconstructor m = GYDatumHash -> m RewardsStep
