{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module GeniusYield.Api.HotKeyRewards.Types
  ( LastRewardsAction (..)
  , RewardsDatum (..)
  , RewardsInfo (..)
  , Rewardee (..)
  , Rewards (..)
  , GYRewardsException (..)
  , RewardsStep (..)
  , Reconstructor
  )
where

import Data.Aeson (object, withObject, (.:), (.=))
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
  , pubKeyHashFromPlutus
  , pubKeyHashToPlutus
  )
import PlutusLedgerApi.V1 (DatumHash, TokenName)
import PlutusLedgerApi.V2 (PubKeyHash)
import PlutusTx (unstableMakeIsData)
import PlutusTx.Builtins.Internal (BuiltinByteString (..))

import GeniusYield.Scripts.MerkleRewards.Hashable (Hash, Hashable)

data LastRewardsAction = RewardsPlaced !Hash !BuiltinByteString | RewardsWithdrawn !PubKeyHash
  deriving (Eq, Ord, Show)

unstableMakeIsData ''LastRewardsAction

placedTag, withdrawnTag :: String
placedTag = "placed"
withdrawnTag = "withdrawn"

instance ToJSON LastRewardsAction where
  toJSON (RewardsPlaced h s) =
    object
      [ "tag" .= placedTag
      , "hash" .= h
      , "url" .= let BuiltinByteString bs = s in decodeUtf8Lenient bs
      ]
  toJSON (RewardsWithdrawn pkh) =
    object
      [ "tag" .= withdrawnTag
      , "key" .= fromRight (error "invalid pkh") (pubKeyHashFromPlutus pkh)
      ]

instance FromJSON LastRewardsAction where
  parseJSON = withObject "LastRewardsAction" $ \o -> do
    tag <- o .: "tag"
    case tag of
      t
        | t == placedTag ->
            RewardsPlaced
              <$> o .: "hash"
              <*> (f <$> o .: "url")
        | t == withdrawnTag -> RewardsWithdrawn . pubKeyHashToPlutus <$> o .: "key"
        | otherwise -> fail $ "invalid tag: " <> t
    where
      f :: Text -> BuiltinByteString
      f = BuiltinByteString . encodeUtf8

data RewardsDatum = RewardsDatum
  { rdInfo :: !BuiltinByteString
  -- ^ Information about these rewards.
  , rdPrevious :: !(Maybe DatumHash)
  -- ^ The hash of the previous datum.
  , rdNFT :: !TokenName
  -- ^ The name of the NFT identifying these Rewards.
  , rdLastAction :: !LastRewardsAction
  -- ^ The last action leading to this state.
  , rdHash :: !Hash
  -- ^ The hash of the currently available rewards.
  }
  deriving (Eq, Ord, Show)

unstableMakeIsData ''RewardsDatum

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
  , riLastAction :: !LastRewardsAction
  -- ^ The last action leading to this state.
  , riHash :: !Hash
  -- ^ The hash of the currently available rewards.
  }
  deriving (Eq, Ord, Show)

instance PrintfArg RewardsInfo where
  formatArg ri = formatArg (show ri)

data Rewardee = WalletRewardee !GYStakeKeyHash | BotRewardee !GYPubKeyHash
  deriving (Eq, Ord, Show)

newtype Rewards = Rewards {getRewards :: Map GYPubKeyHash GYValue}
  deriving stock (Eq, Ord, Show)
  deriving newtype Hashable

instance Semigroup Rewards where
  (<>) :: Rewards -> Rewards -> Rewards
  Rewards a <> Rewards b = Rewards $ Map.unionWith (<>) a b

instance Monoid Rewards where
  mempty :: Rewards
  mempty = Rewards mempty

instance ToJSON Rewards where
  toJSON = toJSON . Map.toList . getRewards

instance FromJSON Rewards where
  parseJSON = fmap (Rewards . Map.fromList) . parseJSON

data GYRewardsException
  = GYInvalidClaim !Rewards !Rewardee
  | GYNoRewardsFor !Rewardee
  | GYRewardsMismatch !Hash !Rewards
  deriving (Eq, Exception, IsGYApiError, Ord, Show)

data RewardsStep
  = RSPlaced !Rewards !(Maybe GYDatumHash)
  | RSWithdrawn !GYPubKeyHash !GYDatumHash
  | RSAcc !Rewards
  deriving (Eq, FromJSON, Generic, Ord, Show, ToJSON)

type Reconstructor m = GYDatumHash -> m RewardsStep
