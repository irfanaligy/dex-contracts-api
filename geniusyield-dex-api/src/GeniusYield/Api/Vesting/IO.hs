module GeniusYield.Api.Vesting.IO
  ( vestingConfigIO
  , recipientsIO
  , vestingAddressIO
  , allVestingInfosIO
  , unlockableVestingInfosIO
  , placeVestingsIO
  , placeRecipientsProgressIO
  , deployVestingValidatorIO
  , cancelVestingsIO
  , unlockVestingsIO
  , safeVestingInfoIO
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Csv qualified as Csv
import Data.Either (rights)
import Data.List.NonEmpty qualified as NE
import Data.List.Split qualified as LS
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GeniusYield.GYConfig
import GeniusYield.Imports
import GeniusYield.Transaction (GYCoinSelectionStrategy (GYLegacy, GYRandomImproveMultiAsset))
import GeniusYield.TxBuilder
import GeniusYield.Types
import Web.HttpApiData qualified as Web

import GeniusYield.Api.OneWay
import GeniusYield.Api.Utils
  ( runGYTxMonadNodeF
  , runGYTxMonadNodeParallel
  , runGYTxMonadNodeParallelWithStrategy
  )
import GeniusYield.Api.Vesting.Operations
import GeniusYield.Api.Vesting.Types

placeRecipientsProgressIO :: FilePath -> IO [GYTxOutRef]
placeRecipientsProgressIO fp = do
  e <- try (T.lines <$> T.readFile fp) >>= handleFile
  let txRefs = map (Web.parseUrlPiece @GYTxOutRef) e
  pure $ rights txRefs
  where
    handleFile :: Either IOError [T.Text] -> IO [T.Text]
    handleFile (Left _) = pure []
    handleFile (Right cnt) = pure cnt

checkPlacedVestingProgressIO
  :: GYProviders
  -> GYNetworkId
  -> [Recipient]
  -> [GYTxOutRef]
  -> IO [Recipient]
checkPlacedVestingProgressIO providers nid recipients placedRecipients = fmap filterRecipients recipientInfoFromRef
  where
    uniqueTxRef :: Set GYTxOutRef
    uniqueTxRef = Set.fromList placedRecipients

    recipientInfoFromRef :: IO (Map GYAddress GYValue)
    recipientInfoFromRef = do
      utxos <- gyQueryUtxosAtTxOutRefsWithDatums providers $ Set.toList uniqueTxRef
      vestingInfosFromRef <- runGYTxQueryMonadIO nid providers $ vestingInfos utxos
      let recipientsInfo = map recipientFromVestingInfo $ Map.elems vestingInfosFromRef
      pure $ Map.fromList recipientsInfo

    recipientFromVestingInfo :: VestingInfo -> (GYAddress, GYValue)
    recipientFromVestingInfo VestingInfo {..} =
      let (rAsset, rAmt) = head $ valueToList viValue
      in (viRecipient, valueSingleton rAsset rAmt)

    filterRecipients :: Map GYAddress GYValue -> [Recipient]
    filterRecipients mRecpAddr =
      let matchedRecp Recipient {..} = Map.lookup recAddress mRecpAddr == Just (valueSingleton recToken (toInteger recAmount))
      in filter (not . matchedRecp) recipients

vestingConfigIO :: HasCallStack => FilePath -> IO VestingConfig
vestingConfigIO file = do
  e <- Aeson.eitherDecodeFileStrict' file
  case e of
    Left err -> throwIO $ userError err
    Right cfg -> return cfg

recipientsIO :: HasCallStack => FilePath -> IO [Recipient]
recipientsIO file = do
  bs <- LBS8.readFile file
  case Csv.decode Csv.NoHeader bs of
    Left err -> throwIO $ userError err
    Right rs -> return $ toList rs

safeVestingInfoIO :: HasCallStack => FilePath -> IO [VestingInfo]
safeVestingInfoIO file = do
  bs <- LBS8.readFile file
  case Csv.decode Csv.NoHeader bs of
    Left _ -> pure []
    Right rs -> return $ toList rs

newtype VestingQuery = VestingQuery {runQuery :: forall a. GYTxQueryMonadIO a -> IO a}

newtype VestingSubmit = VestingSubmit {runSubmitF :: forall a. GYTxBuilderMonadIO (a, GYTxSkeleton PlutusV2) -> IO (a, GYTxId)}

runSubmit :: VestingSubmit -> GYTxSkeleton PlutusV2 -> IO GYTxId
runSubmit s m = snd <$> runSubmitF s (return ((), m))

withVesting
  :: HasCallStack
  => GYCoreConfig
  -> VestingConfig
  -> (GYProviders -> GYNetworkId -> GYPaymentSigningKey -> GYAddress -> GYTxOutRef -> VestingQuery -> VestingSubmit -> (GYLogSeverity -> String -> IO ()) -> IO a)
  -> IO a
withVesting coreCfg VestingConfig {..} act = withCfgProviders coreCfg "vesting" $ \providers -> do
  let
    nid = cfgNetworkId coreCfg
    query = VestingQuery $ runGYTxQueryMonadIO nid providers

  skey <- readPaymentSigningKey vcSKeyFile
  let addr = addressFromPaymentKeyHash nid $ paymentKeyHash $ paymentVerificationKey skey
  gyLogDebug providers "" $ printf "distributor address: %s" addr

  (collateral, n) <- runQuery query $ getCollateral addr 5_000_000
  gyLogDebug providers "" $ printf "collateral at %s (%d lovelace)" collateral n

  let submit = VestingSubmit $ \m -> do
        (a, txBody) <- runGYTxMonadNodeF GYRandomImproveMultiAsset nid providers [addr] addr (Just (collateral, True)) m
        tid <- gySubmitTx providers $ signGYTxBody txBody [skey]
        gyLogInfo providers "" $ printf "submitted tx: %s" tid
        return (a, tid)

  act providers nid skey addr collateral query submit $ gyLog providers ""

vestingAddressIO :: GYCoreConfig -> IO GYAddress
vestingAddressIO coreCfg = withCfgProviders coreCfg "vesting" $ \providers ->
  do runGYTxQueryMonadIO (cfgNetworkId coreCfg) providers vestingAddress

allVestingInfosIO :: GYCoreConfig -> IO (Map GYTxOutRef VestingInfo)
allVestingInfosIO coreCfg@GYCoreConfig {..} =
  withCfgProviders coreCfg "vesting" $ \providers -> do
    runGYTxQueryMonadIO cfgNetworkId providers allVestingInfos

unlockableVestingInfosIO :: GYCoreConfig -> IO (Map GYTxOutRef VestingInfo)
unlockableVestingInfosIO coreCfg =
  withCfgProviders coreCfg "vesting" $ \providers -> do
    runGYTxQueryMonadIO (cfgNetworkId coreCfg) providers unlockableVestingInfos

placeVestingsIO
  :: HasCallStack
  => GYCoreConfig
  -> VestingConfig
  -> [GYTxOutRef]
  -> [Recipient]
  -> ([GYTxOutRef] -> IO ())
  -> Maybe Int
  -> IO ()
placeVestingsIO coreCfg vc rsProgress rs appendProgress mbatchCnt = withVesting coreCfg vc $ \providers nid skey addr c q _ l -> do
  vAddr <- runQuery q vestingAddress
  l GYDebug $ printf "placing vestings on %s at vesting script address %s" (show nid) vAddr
  let
    cancelKey = pubKeyHash $ paymentVerificationKey skey
    deposit = addressFromPaymentKeyHash nid (fromPubKeyHash cancelKey)
  filteredRecipients <- checkPlacedVestingProgressIO providers (cfgNetworkId coreCfg) rs rsProgress

  gyLogInfo providers "" $ printf "\n Total Recipient to Fill: %d" (length filteredRecipients)

  handlePlaceVesting providers c addr deposit cancelKey filteredRecipients skey (fromMaybe count mbatchCnt)
  where
    count = 60

    handlePlaceVesting
      :: ToShelleyWitnessSigningKey k
      => -- \| The providers.
      GYProviders
      -- \| Collateral UTXO
      -> GYTxOutRef
      -- \| The Tx signer address.
      -> GYAddress
      -- \| The Deposit Address.
      -> GYAddress
      -- \| The Cancel PubKeyHash.
      -> GYPubKeyHash
      -- \| The Recipients.
      -> [Recipient]
      -- \| The Payment signing Key.
      -> k
      -- \| Batch Size
      -> Int
      -> IO ()
    handlePlaceVesting providers _ _ _ _ [] _ _ = gyLogInfo providers "" $ printf "completed placing vesting"
    handlePlaceVesting providers collateral addrs depositAddr cancelKey recipients skey batchCnt = do
      let
        chunks = LS.chunksOf batchCnt recipients
        toFill = take 5 chunks

      txBdyFill <- runGYTxMonadNodeParallel (cfgNetworkId coreCfg) providers [addrs] addrs (Just (collateral, True)) $ traverse (placeVestings depositAddr cancelKey) toFill

      let allTxRefs = concatMap (utxosRefs . txBodyUTxOs) $ extractBody txBdyFill
      putStrLn $ "\n total body: " <> show (length allTxRefs)

      gyLogInfo providers "" "Saving TxRefs ... "
      appendProgress allTxRefs
      txResult <- handleTxBuildResult providers skey txBdyFill
      case txResult of
        Left (GYTxBuildFailure val) -> do
          gyLogInfo providers "place vesting" $ printf "Tx Build failure for value: %s" (show val)
          threadDelay 100_000_000 -- wait 100s
          handlePlaceVesting providers collateral addrs depositAddr cancelKey recipients skey batchCnt
        Left _ -> gyLogError providers "place vesting" "Error: No Build Inputs"
        Right txIds -> do
          let nextFill = concat $ drop (length txIds) chunks
          gyLogInfo providers "" $ printf "completed placing vesting for \n %s" (show txIds)
          gyLogInfo providers "" $ printf "\n Waiting for another tx \n"
          threadDelay 100_000_000 -- wait 100s
          handlePlaceVesting providers collateral addrs depositAddr cancelKey nextFill skey batchCnt

cancelVestingsIO
  :: HasCallStack
  => GYCoreConfig
  -> Maybe GYTxOutRef
  -> VestingConfig
  -> [GYTxOutRef]
  -> Maybe Int
  -> IO ()
cancelVestingsIO coreCfg mscriptRef vc refs mBatchCnt = withVesting coreCfg vc $ \providers _nid skey addrs collateral _q _s _l -> do
  gyLogDebug providers "cancel vesting" $ printf "cancelling vestings at %s" $ show refs
  utxosWithDatums <- runGYTxQueryMonadIO (cfgNetworkId coreCfg) providers $ utxosAtTxOutRefsWithDatums refs
  infos <- runGYTxQueryMonadIO (cfgNetworkId coreCfg) providers $ vestingInfos utxosWithDatums
  handleMultiCancelTx providers collateral addrs skey (Map.elems infos) (fromMaybe count mBatchCnt)
  where
    count = 5

    handleMultiCancelTx
      :: ToShelleyWitnessSigningKey k
      => -- \| The providers
      GYProviders
      -- \| Collateral Utxo
      -> GYTxOutRef
      -- \| Tx Signer Address
      -> GYAddress
      -- \| Payment Signing Key
      -> k
      -- \| Vesting Infos
      -> [VestingInfo]
      -- \| Batch Count
      -> Int
      -> IO ()
    handleMultiCancelTx providers _ _ _ [] _ = gyLogInfo providers "" $ printf "completed Cancelling vesting"
    handleMultiCancelTx providers collateral addrs skey vInfos batchCnt = do
      let
        chunks = LS.chunksOf batchCnt vInfos
        toFill = take 5 chunks

      txBdyFill <- runGYTxMonadNodeParallel (cfgNetworkId coreCfg) providers [addrs] addrs (Just (collateral, True)) $ traverse (cancelVestings mscriptRef) toFill
      txResult <- handleTxBuildResult providers skey txBdyFill

      case txResult of
        Left (GYTxBuildFailure val) -> do
          gyLogInfo providers "cancel vesting" $ printf "Tx Build failure for value: %s" (show val)
          threadDelay 100_000_000 -- wait 100s
          handleMultiCancelTx providers collateral addrs skey vInfos batchCnt
        Left _ -> gyLogError providers "place vesting" "Error: No Build Inputs"
        Right txIds -> do
          let nextFill = concat $ drop (length txIds) chunks
          gyLogInfo providers "" $ printf "completed canceling vesting for \n %s" (show txIds)
          gyLogInfo providers "" $ printf "\n Waiting for another tx \n"
          threadDelay 100_000_000 -- wait 100s
          handleMultiCancelTx providers collateral addrs skey nextFill batchCnt

deployVestingValidatorIO :: HasCallStack => GYCoreConfig -> VestingConfig -> IO GYTxOutRef
deployVestingValidatorIO coreCfg vc = withVesting coreCfg vc $ \providers _nid _skey _usrAddr _ q s l -> do
  tid <- runSubmit s deployVestingValidator
  void $ gyWaitForNextBlock providers
  addr <- runQuery q $ pure oneWayAddress
  utxos <- gyQueryUtxosAtAddress providers addr Nothing
  case find (f tid) $ utxosToList utxos of
    Nothing -> throwIO $ userError "UTxO holding reference script not found"
    Just utxo -> do
      let ref = utxoRef utxo
      l GYInfo $ printf "deployed vesting validator at %s" ref
      return ref
  where
    f :: GYTxId -> GYUTxO -> Bool
    f tid GYUTxO {..} = fst (txOutRefToTuple utxoRef) == tid && isJust utxoRefScript

-- | Claim vesting
unlockVestingsIO
  :: HasCallStack
  => GYCoreConfig
  -- ^ The Core config provider
  -> Maybe GYTxOutRef
  -- ^ The Vesting info ReferenceScript
  -> VestingConfig
  -- ^ The Vesting Config
  -> [GYTxOutRef]
  -- ^  The Vesting Utxo Refs
  -> Maybe Int
  -- ^ Batch Size
  -> IO ()
unlockVestingsIO coreCfg@GYCoreConfig {..} mScriptRef vc refs maybeBatch =
  withVesting coreCfg vc $ \providers _nid skey addr collateral _q _s _l -> do
    gyLogDebug providers "" $ printf "unlocking Vesting for \n %s" (show refs)
    utxosWithDatums <- runGYTxQueryMonadIO cfgNetworkId providers $ utxosAtTxOutRefsWithDatums refs
    infos <- runGYTxQueryMonadIO cfgNetworkId providers $ vestingInfos utxosWithDatums
    handleUnlockMulti providers [addr] collateral skey (Map.elems infos) (fromMaybe count maybeBatch)
  where
    count = 4

    handleUnlockMulti
      :: ToShelleyWitnessSigningKey k
      => -- \| Providers
      GYProviders
      -- \| Signer Address
      -> [GYAddress]
      -- \| Signer Collateral
      -> GYTxOutRef
      -- \| Signing Key
      -> k
      -- \| Vesting Info to unlock
      -> [VestingInfo]
      -- \| Batch Count
      -> Int
      -> IO ()

    handleUnlockMulti providers _ _ _ [] _ = gyLogInfo providers "" $ printf "completed placing vesting"
    handleUnlockMulti providers addrs collateral sKey vInfos batchCnt = do
      let
        chunks = LS.chunksOf batchCnt vInfos
        toFill = take 5 chunks

      txBdyFill <- runGYTxMonadNodeParallelWithStrategy GYLegacy cfgNetworkId providers addrs (head addrs) (Just (collateral, True)) $ traverse (unlockVestings mScriptRef) toFill
      txResult <- handleTxBuildResult providers sKey txBdyFill
      case txResult of
        Left (GYTxBuildFailure val) -> do
          gyLogInfo providers "" $ printf "Tx Build failure for value: %s" (show val)
          threadDelay 100_000_000 -- wait 100s
          handleUnlockMulti providers addrs collateral sKey vInfos batchCnt
        Left _ -> gyLogError providers "claim vesting" "Error: No Build Inputs"
        Right txIds -> do
          let nextFill = concat $ drop (length txIds) chunks
          gyLogInfo providers "" $ printf "completed placing vesting for \n %s" (show txIds)
          gyLogInfo providers "" $ printf "\n Waiting for another tx \n"
          threadDelay 100_000_000 -- wait 100s
          handleUnlockMulti providers addrs collateral sKey nextFill batchCnt

handleTxBuildResult
  :: ToShelleyWitnessSigningKey k
  => GYProviders
  -> k
  -> GYTxBuildResult
  -> IO (Either GYTxBuildResult [GYTxId])
handleTxBuildResult _ _ r@(GYTxBuildFailure _) = pure $ Left r
handleTxBuildResult _ _ r@GYTxBuildNoInputs = pure $ Left r
handleTxBuildResult providers skey buildResults = do
  txIds <- liftIO $ traverse (\b -> gySubmitTx providers $ signGYTxBody b [skey]) (extractBody buildResults)
  pure $ Right txIds

extractBody :: GYTxBuildResult -> [GYTxBody]
extractBody (GYTxBuildSuccess bodies) = NE.toList bodies
extractBody (GYTxBuildPartialSuccess _ bodies) = NE.toList bodies
extractBody _ = []
