module GeniusYield.Api.HotKeyRewards.IO
  ( ReconstructorIO (..)
  , RewardsConfig (..)
  , ConstructRewardsConfig (..)
  , ActorConfig (..)
  , downloadRewards
  , queryRewardsIO
  , constructRewardsFIO
  , constructRewardsIO
  , writeRewards
  , readRewards
  , writeRewards'
  , reconstructorIO
  , cachedReconstructorIO
  , allRewardsInfosIO
  , allRewardsForRewardeeIO
  , signAndSubmit
  , placeRewardsIO
  , topUpRewardsIO
  , clawBackRewardsIO
  , declutterRewardsIO
  , withdrawRewardsIO
  , withdrawAllRewardsIO
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Aeson (eitherDecodeFileStrict', encodeFile)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import GeniusYield.GYConfig (GYCoreConfig (cfgNetworkId), withCfgProviders)
import GeniusYield.Imports
  ( HasCallStack
  , IsString (fromString)
  , Map
  , printf
  , throwIO
  , void
  )
import GeniusYield.Transaction (GYCoinSelectionStrategy (GYRandomImproveMultiAsset))
import GeniusYield.TxBuilder
  ( GYTxBuilderMonadIO
  , GYTxQueryMonad
  , GYTxQueryMonadIO
  , GYTxSkeleton
  , gyLogInfo'
  , runGYTxQueryMonadIO
  )
import GeniusYield.Types
  ( GYAddress
  , GYDatumHash
  , GYPaymentSigningKey
  , GYProviders
  , GYPubKeyHash
  , GYStakeKeyHash
  , GYTxId
  , GYTxOutRef
  , GYValue
  , PlutusVersion (..)
  , gyLogInfo
  , gySubmitTx
  , paymentVerificationKey
  , pubKeyHash
  , readPaymentSigningKey
  , signGYTx
  , signGYTxBody
  , tokenNameToHex
  , txBodyTxId
  )
import GeniusYield.Types.Tx (GYTx)
import Network.HTTP.Client (Response (responseBody), parseRequest)
import Network.HTTP.Simple (httpJSON)

import GeniusYield.Api.Cache (Cache, withCache)
import GeniusYield.Api.HotKeyRewards.Operations
  ( allRewardsForRewardee
  , allRewardsInfos
  , clawBackRewards
  , declutterRewards
  , mkReconstructor
  , placeRewards
  , toRewards
  , topUpRewards
  , withdrawAllRewards
  , withdrawRewards
  )
import GeniusYield.Api.HotKeyRewards.Types
  ( Reconstructor
  , Rewardee
  , Rewards (..)
  , RewardsInfo (..)
  , RewardsStep
  )
import GeniusYield.Api.Types (Secret (..))
import GeniusYield.Api.Utils (runGYTxMonadNodeF)
import GeniusYield.Scripts (GYCompiledScripts)

newtype ReconstructorIO = ReconstructorIO {reconstructIO :: forall m. (GYTxQueryMonad m, MonadIO m) => Reconstructor m}

data RewardsConfig = RewardsConfig
  { rcCfg :: !GYCoreConfig
  -- ^ The core configuration.
  , rcOwner :: !GYPubKeyHash
  -- ^ Pubkey hash of the rewards owner.
  , rcReconstructor :: !ReconstructorIO
  -- ^ The reconstructor.
  , rcCache :: !(Maybe (Cache IO GYDatumHash RewardsStep))
  -- ^ Optional cache.
  , rcCompiledScripts :: !GYCompiledScripts
  -- ^ The compiled scripts.
  }

queryRewardsIO
  :: HasCallStack
  => RewardsConfig
  -> (GYCompiledScripts -> ReconstructorIO -> GYPubKeyHash -> GYTxQueryMonadIO a)
  -> IO a
queryRewardsIO RewardsConfig {..} query = do
  let nid = cfgNetworkId rcCfg
  withCfgProviders rcCfg "rewards" $ \providers ->
    runGYTxQueryMonadIO nid providers
      $ query rcCompiledScripts rcReconstructor rcOwner

data ConstructRewardsConfig = ConstructRewardsConfig
  { crcCfg :: !GYCoreConfig
  -- ^ The core configuration.
  , crcOwnerSKey :: !(Secret GYPaymentSigningKey)
  -- ^ The signing key of the owner.
  , crcReconstructor :: !ReconstructorIO
  -- ^ The reconstructor.
  , crcCache :: !(Maybe (Cache IO GYDatumHash RewardsStep))
  -- ^ Optional cache.
  , crcCompiledScripts :: !GYCompiledScripts
  -- ^ The compiled scripts.
  }

data ActorConfig = ActorConfig
  { acAddresses :: ![GYAddress]
  , acChange :: !GYAddress
  , acCollateral :: !(Maybe GYTxOutRef)
  }
  deriving (Eq, Ord, Show)

constructRewardsFIO
  :: HasCallStack
  => ConstructRewardsConfig
  -- ^ The configuration.
  -> ActorConfig
  -- ^ The actor configuration.
  -> ( GYCompiledScripts
       -> ReconstructorIO
       -> GYPubKeyHash
       -> GYTxBuilderMonadIO (a, GYTxSkeleton PlutusV2)
     )
  -> (GYProviders -> a -> GYTxId -> GYTx -> IO b)
  -> IO b
constructRewardsFIO ConstructRewardsConfig {..} ActorConfig {..} mkSkeleton cont = do
  let
    nid = cfgNetworkId crcCfg
    skey = getSecret crcOwnerSKey
    pkh = pubKeyHash $ paymentVerificationKey skey
    mcollateral = (,False) <$> acCollateral

  withCfgProviders crcCfg "rewards" $ \providers -> do
    (a, txBody) <-
      runGYTxMonadNodeF GYRandomImproveMultiAsset nid providers acAddresses acChange mcollateral
        $ mkSkeleton crcCompiledScripts crcReconstructor pkh
    let
      tid = txBodyTxId txBody
      tx' = signGYTxBody txBody [skey]
    cont providers a tid tx'

constructRewardsIO
  :: HasCallStack
  => ConstructRewardsConfig
  -- ^ The configuration.
  -> ActorConfig
  -- ^ The actor configuration.
  -> ( GYCompiledScripts
       -> ReconstructorIO
       -> GYPubKeyHash
       -> GYTxBuilderMonadIO (GYTxSkeleton PlutusV2)
     )
  -> (GYProviders -> GYTxId -> GYTx -> IO a)
  -> IO a
constructRewardsIO crc ac mkSkeleton cont =
  constructRewardsFIO crc ac mkSkeleton'
    $ \providers () tid tx -> cont providers tid tx
  where
    mkSkeleton' gycs r pkh = ((),) <$> mkSkeleton gycs r pkh

writeRewards :: FilePath -> Rewards -> IO ()
writeRewards = encodeFile

readRewards :: FilePath -> IO Rewards
readRewards file = do
  e <- eitherDecodeFileStrict' file
  case e of
    Left err -> throwIO $ userError $ "failed to read rewards from " <> file <> ": " <> err
    Right rs -> pure rs

writeRewards' :: FilePath -> [(GYAddress, GYValue)] -> IO ()
writeRewards' outFile = writeRewards outFile . toRewards

downloadRewards :: String -> IO Rewards
downloadRewards url = do
  req <- parseRequest $ fromString url
  resp <- httpJSON req
  pure $ responseBody resp

allRewardsInfosIO
  :: HasCallStack
  => RewardsConfig
  -- ^ The configuration.
  -> IO (Map GYTxOutRef RewardsInfo)
allRewardsInfosIO cfg = queryRewardsIO cfg $ \gycs _reconstructor pkh -> do
  infos <- allRewardsInfos gycs pkh
  gyLogInfo' "" $ "\n\n" <> unlines (showInfo <$> Map.elems infos)
  pure infos
  where
    showInfo :: RewardsInfo -> String
    showInfo RewardsInfo {..} =
      printf
        "info:     %s\nref:      %s\naddress:  %s\nhash:     %s\nprevious: %s\nNFT:      %s\nlast:     %s\nvalue:    %s\n\n"
        riInfo
        riRef
        riAddress
        riHash
        (maybe ("-" :: String) show riPrevious)
        (tokenNameToHex riNFT)
        (show riLastAction)
        riValue

reconstructorIO :: ReconstructorIO
reconstructorIO = ReconstructorIO $ mkReconstructor $ liftIO . downloadRewards

cachedReconstructorIO :: Cache IO GYDatumHash RewardsStep -> ReconstructorIO
cachedReconstructorIO cache = ReconstructorIO $ withCache cache $ reconstructIO reconstructorIO

allRewardsForRewardeeIO
  :: HasCallStack
  => RewardsConfig
  -- ^ The configuration.
  -> Rewardee
  -- ^ The rewardee.
  -> IO (Map GYTxOutRef (RewardsInfo, GYValue))
allRewardsForRewardeeIO cfg rewardee = queryRewardsIO cfg $ \gycs reconstructor pkh -> do
  m <- flip runReaderT (rcCache cfg) $ allRewardsForRewardee gycs (reconstructIO reconstructor) rewardee pkh
  gyLogInfo' "" $ "list of all rewards for rewardee: " <> show rewardee <> "\n\n" <> unlines (showLine <$> Map.toList m)
  pure m
  where
    showLine :: (GYTxOutRef, (RewardsInfo, GYValue)) -> String
    showLine (ref, (info, value)) = printf "%s (%s): %s\n\n" ref (riInfo info) value

signAndSubmit
  :: HasCallStack
  => GYPaymentSigningKey
  -> GYProviders
  -> GYTxId
  -> GYTx
  -> IO GYTxId
signAndSubmit skey providers _tid tx = do
  let tx' = signGYTx tx [skey]
  tid <- gySubmitTx providers tx'
  gyLogInfo providers "" $ printf "\n\nsubmitted tx: %s\n\n" tid
  pure tid

placeRewardsIO
  :: HasCallStack
  => ConstructRewardsConfig
  -- ^ The rewards configuration.
  -> ActorConfig
  -- ^ The actor configuration.
  -> FilePath
  -- ^ The path to the actor's signing key.
  -> Maybe GYStakeKeyHash
  -- ^ Optional stake key hash to use.
  -> String
  -- ^ Information describing these rewards.
  -> String
  -- ^ The URL containing the rewards information.
  -> IO GYTxId
placeRewardsIO crc ac skeyFile mskh info url = do
  skey <- readPaymentSigningKey skeyFile
  rs <- downloadRewards url

  constructRewardsIO
    crc
    ac
    ( \gycs _reconstructor ownerPkh -> do
        gyLogInfo' "" $ printf "\nplacing rewards for %s:\n%s\n" ownerPkh (show rs)
        placeRewards gycs ownerPkh mskh (Text.pack info) url rs
    )
    (signAndSubmit skey)

topUpRewardsIO
  :: HasCallStack
  => ConstructRewardsConfig
  -- ^ The rewards configuration.
  -> ActorConfig
  -- ^ The actor configuration.
  -> FilePath
  -- ^ The path to the actor's signing key.
  -> GYTxOutRef
  -- ^ The reference to the rewards to top up.
  -> String
  -- ^ Information describing these rewards.
  -> String
  -- ^ The URL containing the rewards information.
  -> IO GYTxId
topUpRewardsIO crc ac skeyFile ref info url = do
  skey <- readPaymentSigningKey skeyFile
  rs <- downloadRewards url

  constructRewardsIO
    crc
    ac
    ( \gycs reconstructor ownerPkh -> do
        gyLogInfo' "" $ printf "\ntopping up rewards for %s:\n%s\n" ownerPkh (show rs)
        flip runReaderT (crcCache crc) $ topUpRewards gycs (reconstructIO reconstructor) ownerPkh ref (Text.pack info) url rs
    )
    (signAndSubmit skey)

clawBackRewardsIO
  :: HasCallStack
  => ConstructRewardsConfig
  -- ^ The rewards configuration.
  -> ActorConfig
  -- ^ The actor configuration.
  -> FilePath
  -- ^ The path to the actor's signing key.
  -> GYTxOutRef
  -- ^ The reference to the rewards to claw back.
  -> IO (GYTxId, Rewards)
clawBackRewardsIO crc ac skeyFile ref = do
  skey <- readPaymentSigningKey skeyFile

  constructRewardsFIO
    crc
    ac
    ( \gycs reconstructor ownerPkh -> do
        gyLogInfo' "" $ printf "\nclawing back rewards for %s from %s" ownerPkh ref
        flip runReaderT (crcCache crc) $ clawBackRewards gycs (reconstructIO reconstructor) ownerPkh ref
    )
    ( \providers rs tid tx -> do
        void $ signAndSubmit skey providers tid tx
        pure (tid, rs)
    )

declutterRewardsIO
  :: HasCallStack
  => ConstructRewardsConfig
  -- ^ The rewards configuration.
  -> ActorConfig
  -- ^ The actor configuration.
  -> FilePath
  -- ^ The path to the actor's signing key.
  -> IO GYTxId
declutterRewardsIO crc ac skeyFile = do
  skey <- readPaymentSigningKey skeyFile

  constructRewardsIO
    crc
    ac
    ( \gycs _reconstructor ownerPkh -> do
        gyLogInfo' "" $ printf "\ndecluttering rewards for %s" ownerPkh
        declutterRewards gycs ownerPkh
    )
    (signAndSubmit skey)

withdrawRewardsIO
  :: HasCallStack
  => ConstructRewardsConfig
  -- ^ The rewards configuration.
  -> ActorConfig
  -- ^ The actor configuration for the rewardee.
  -> Rewardee
  -- ^ The rewardee.
  -> GYTxOutRef
  -- ^ The reference to the Rewards to withdraw from.
  -> IO GYTx
withdrawRewardsIO crc ac rewardee ref =
  constructRewardsIO
    crc
    ac
    ( \gycs reconstructor ownerPkh -> do
        gyLogInfo' "" $ printf "\nwithdrawing rewards for %s from %s" (show rewardee) ref
        flip runReaderT (crcCache crc) $ withdrawRewards gycs (reconstructIO reconstructor) ownerPkh ref rewardee
    )
    ( \providers tid tx -> do
        gyLogInfo providers "" $ printf "\n\nconstructed withdrawal tx: %s" tid
        pure tx
    )

withdrawAllRewardsIO
  :: HasCallStack
  => ConstructRewardsConfig
  -- ^ The rewards configuration.
  -> ActorConfig
  -- ^ The actor configuration for the rewardee.
  -> Rewardee
  -- ^ The rewardee.
  -> IO GYTx
withdrawAllRewardsIO crc ac rewardee =
  constructRewardsIO
    crc
    ac
    ( \gycs reconstructor ownerPkh -> do
        gyLogInfo' "" $ printf "\nwithdrawing all rewards for %s" $ show rewardee
        flip runReaderT (crcCache crc) $ withdrawAllRewards gycs (reconstructIO reconstructor) ownerPkh rewardee
    )
    ( \providers tid tx -> do
        gyLogInfo providers "" $ printf "\n\nconstructed withdrawal tx: %s" tid
        pure tx
    )
