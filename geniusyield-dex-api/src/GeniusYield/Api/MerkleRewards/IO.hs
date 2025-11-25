module GeniusYield.Api.MerkleRewards.IO
  ( ReconstructorIO (..)
  , RewardsConfig (..)
  , SubmitRewardsConfig (..)
  , queryRewardsIO
  , submitRewardsFIO
  , submitRewardsIO
  , writeRewards
  , reconstructorIO
  , cachedReconstructorIO
  , allRewardsInfosIO
  , allRewardsForRewardeeIO
  , deployRewardsScriptIO
  , placeRewardsIO
  , clawBackRewardsIO
  , withdrawRewardsBotIO
  , withdrawAllRewardsBotIO
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..))
import Data.Aeson (encodeFile)
import Data.Map.Strict qualified as Map
import GeniusYield.GYConfig (GYCoreConfig (cfgNetworkId), coreConfigIO, withCfgProviders)
import GeniusYield.Imports (HasCallStack, IsString (fromString), Text, printf)
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
  ( AsPubKeyHash (toPubKeyHash)
  , GYAddress
  , GYDatumHash
  , GYPubKeyHash
  , GYStakeKeyHash
  , GYTxId
  , GYTxOutRef
  , GYValue
  , PlutusVersion (..)
  , addressFromPaymentKeyHash
  , gyLogInfo
  , gySubmitTx
  , paymentKeyHash
  , paymentVerificationKey
  , readPaymentSigningKey
  , signGYTxBody
  , tokenNameToHex
  )
import Network.HTTP.Client (Response (responseBody), parseRequest)
import Network.HTTP.Simple (httpJSON)

import GeniusYield.Api.Cache (Cache, withCache)
import GeniusYield.Api.MerkleRewards.Operations
  ( allRewardsForRewardee
  , allRewardsInfos
  , clawBackRewards
  , deployRewardsScript
  , mkReconstructor
  , placeRewards
  , rewardsToMerkleTree
  , toRewards
  , withdrawAllRewards
  , withdrawRewards
  )
import GeniusYield.Api.MerkleRewards.Types
  ( Reconstructor
  , Rewardee (BotRewardee)
  , Rewards (..)
  , RewardsInfo (..)
  , RewardsStep
  )
import GeniusYield.Api.Utils (runGYTxMonadNodeF)
import GeniusYield.Scripts (GYCompiledScripts, readCompiledScripts)

newtype ReconstructorIO = ReconstructorIO {reconstructIO :: forall m. (GYTxQueryMonad m, MonadIO m) => Reconstructor m}

data RewardsConfig = RewardsConfig
  { rcCfgFile :: !FilePath
  -- ^ Path to the core configuration.
  , rcOwner :: !GYPubKeyHash
  -- ^ Pubkey hash of the rewards owner.
  , rcReconstructor :: !ReconstructorIO
  -- ^ The reconstructor.
  }

queryRewardsIO
  :: HasCallStack
  => RewardsConfig
  -> ( ReconstructorIO
       -> GYPubKeyHash
       -> ReaderT GYCompiledScripts GYTxQueryMonadIO a
     )
  -> IO a
queryRewardsIO RewardsConfig {..} query = do
  cfg <- coreConfigIO rcCfgFile
  gycs <- readCompiledScripts
  let nid = cfgNetworkId cfg
  withCfgProviders cfg "rewards" $ \providers ->
    runGYTxQueryMonadIO nid providers
      $ flip runReaderT gycs
      $ query rcReconstructor rcOwner

data SubmitRewardsConfig = SubmitRewardsConfig
  { srcCfg :: !RewardsConfig
  -- ^ The configuration.
  , srcSKeyFile :: !FilePath
  -- ^ Path to the signing key.
  , srcCollateral :: !GYTxOutRef
  -- ^ The collateral.
  }

submitRewardsFIO
  :: HasCallStack
  => SubmitRewardsConfig
  -- ^ The configuration.
  -> ( ReconstructorIO
       -> GYPubKeyHash
       -> GYAddress
       -> GYPubKeyHash
       -> ReaderT GYCompiledScripts GYTxBuilderMonadIO (a, GYTxSkeleton PlutusV2)
     )
  -> IO (a, GYTxId)
submitRewardsFIO SubmitRewardsConfig {..} mkSkeleton = do
  let RewardsConfig {..} = srcCfg

  cfg <- coreConfigIO rcCfgFile
  skey <- readPaymentSigningKey srcSKeyFile
  gycs <- readCompiledScripts

  let
    nid = cfgNetworkId cfg
    pkh = paymentKeyHash $ paymentVerificationKey skey
    addr = addressFromPaymentKeyHash nid pkh

  withCfgProviders cfg "rewards" $ \providers -> do
    (a, txBody) <-
      runGYTxMonadNodeF GYRandomImproveMultiAsset nid providers [addr] addr (Just (srcCollateral, False))
        $ flip runReaderT gycs
        $ mkSkeleton rcReconstructor rcOwner addr (toPubKeyHash pkh)
    let tx = signGYTxBody txBody [skey]
    tid <- gySubmitTx providers tx
    gyLogInfo providers "" $ printf "\n\nsubmitted tx: %s" tid
    pure (a, tid)

submitRewardsIO
  :: HasCallStack
  => SubmitRewardsConfig
  -- ^ The configuration.
  -> ( ReconstructorIO
       -> GYPubKeyHash
       -> GYAddress
       -> GYPubKeyHash
       -> ReaderT GYCompiledScripts GYTxBuilderMonadIO (GYTxSkeleton PlutusV2)
     )
  -> IO GYTxId
submitRewardsIO cfg mkSkeleton = do
  ((), tid) <- submitRewardsFIO cfg $ \reconstructor ownerPkh addr pkh -> do
    s <- mkSkeleton reconstructor ownerPkh addr pkh
    pure ((), s)
  pure tid

writeRewards :: FilePath -> [(GYAddress, GYValue)] -> IO ()
writeRewards outFile = encodeFile outFile . toRewards

downloadRewards :: String -> IO Rewards
downloadRewards url = do
  req <- parseRequest $ fromString url
  resp <- httpJSON req
  pure $ responseBody resp

allRewardsInfosIO
  :: HasCallStack
  => RewardsConfig
  -- ^ The configuration.
  -> IO ()
allRewardsInfosIO cfg = queryRewardsIO cfg $ \_reconstructor pkh -> do
  infos <- allRewardsInfos pkh
  gyLogInfo' "" $ "\n\n" <> unlines (showInfo <$> Map.elems infos)
  where
    showInfo :: RewardsInfo -> String
    showInfo RewardsInfo {..} =
      printf
        "info:     %s\nref:      %s\naddress:  %s\nprevious: %s\nNFT:      %s\nroot:     %s\ndepth:    %d\nlast:     %s\nvalue:    %s\n\n"
        riInfo
        riRef
        riAddress
        (maybe ("-" :: String) show riPrevious)
        (tokenNameToHex riNFT)
        riRoot
        riDepth
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
  -> IO ()
allRewardsForRewardeeIO cfg rewardee = queryRewardsIO cfg $ \reconstructor pkh -> do
  m <- allRewardsForRewardee (reconstructIO reconstructor) rewardee pkh
  gyLogInfo' "" $ "\n\n" <> unlines (showLine <$> Map.toList m)
  where
    showLine :: (GYTxOutRef, (Text, GYValue)) -> String
    showLine (ref, (info, value)) = printf "%s (%s): %s\n\n" ref info value

deployRewardsScriptIO
  :: HasCallStack
  => SubmitRewardsConfig
  -- ^ The configuration.
  -> IO GYTxId
deployRewardsScriptIO cfg = submitRewardsIO cfg $ \_reconstructor _ownerPkh _addr pkh -> do
  gyLogInfo' "" $ printf "\ndeploying Rewards script for %s" pkh
  deployRewardsScript pkh

placeRewardsIO
  :: HasCallStack
  => SubmitRewardsConfig
  -- ^ The configuration.
  -> Maybe GYStakeKeyHash
  -- ^ Optional stake key hash to use.
  -> String
  -- ^ Information describing these rewards.
  -> String
  -- ^ The URL containing the rewards information.
  -> IO GYTxId
placeRewardsIO cfg mskh info url = do
  rs <- downloadRewards url

  let
    tn = fromString info
    tree = rewardsToMerkleTree rs

  submitRewardsIO cfg $ \_reconstructor _ownerPkh _addr pkh -> do
    gyLogInfo' "" $ printf "\nplacing rewards for %s:\n%s\n" pkh (show rs)
    placeRewards pkh mskh tn url tree

clawBackRewardsIO
  :: HasCallStack
  => SubmitRewardsConfig
  -- ^ The configuration.
  -> Maybe GYTxOutRef
  -- ^ Optional reference to the Rewards script.
  -> GYTxOutRef
  -- ^ The reference to the Rewards to claw back.
  -> IO GYTxId
clawBackRewardsIO cfg msref ref = submitRewardsIO cfg $ \_reconstructor _ownerPkh _addr pkh -> do
  gyLogInfo' "" $ printf "\nclawing back rewards for %s from %s" pkh ref
  clawBackRewards pkh msref ref

withdrawRewardsBotIO
  :: HasCallStack
  => SubmitRewardsConfig
  -- ^ The configuration.
  -> Maybe GYTxOutRef
  -- ^ Optional reference to the Rewards script.
  -> GYTxOutRef
  -- ^ The reference to the Rewards to claw back.
  -> IO GYTxId
withdrawRewardsBotIO cfg msref ref = submitRewardsIO cfg $ \reconstructor ownerPkh botAddr botPkh -> do
  gyLogInfo' "" $ printf "\nwithdrawing rewards for %s from %s" botPkh ref
  withdrawRewards (reconstructIO reconstructor) ownerPkh msref ref (BotRewardee botPkh) botAddr

withdrawAllRewardsBotIO
  :: HasCallStack
  => SubmitRewardsConfig
  -- ^ The configuration.
  -> Maybe GYTxOutRef
  -- ^ Optional reference to the Rewards script.
  -> IO GYTxId
withdrawAllRewardsBotIO cfg msref = submitRewardsIO cfg $ \reconstructor ownerPkh botAddr botPkh -> do
  gyLogInfo' "" $ printf "\nwithdrawing all rewards for %s" botPkh
  withdrawAllRewards (reconstructIO reconstructor) ownerPkh msref (BotRewardee botPkh) botAddr
