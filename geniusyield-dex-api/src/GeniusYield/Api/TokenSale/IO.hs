{-# LANGUAGE OverloadedStrings #-}

module GeniusYield.Api.TokenSale.IO
  ( ordersIO
  , calculateDistributionIO
  , distributeIO
  , WhiteListInfo (..)
  , TsDistributionLog (..)
  , distributeMultiTxIO
  , tokenSaleSummaryIO
  , simTokenSaleWhitelistIO
  , dbTokenSaleWhitelistIO
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Csv qualified as Csv
import Data.List.NonEmpty qualified as NE
import Data.List.Split qualified as LS
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Text qualified as T
import Database.PostgreSQL.Simple
import GeniusYield.Imports
import GeniusYield.TxBuilder
import GeniusYield.Types

import GeniusYield.Api.TokenSale.ChainAnalysis
import GeniusYield.Api.TokenSale.Distribution
import GeniusYield.Api.TokenSale.Operations
import GeniusYield.Api.TokenSale.Types
import GeniusYield.Api.TokenSale.Utils
import GeniusYield.Api.Utils (runGYTxMonadNode, runGYTxMonadNodeParallel)
import GeniusYield.Scripts

data WhiteListInfo = WhiteListInfo
  { txRef :: GYTxOutRef
  , stakeKeyHash :: String
  }
  deriving (Csv.FromRecord, Csv.ToRecord, Generic, Show)

data TsDistributionLog = TsDistributionLog
  { tdRoundAddr :: !GYAddress --  ^ Round Script Address
  , tdOrderRef :: !GYTxOutRef --  ^ Order UTXO Ref
  , tdStakeKey :: !String --  ^ StakeKey of User
  , tdPayAddr :: !GYAddress --  ^ Payment Address where token will be sent
  , tdAmt :: !Natural --  ^ Token Amount
  , tdTxId :: !String --  ^ Transaction id of token distribution
  }
  deriving stock (Generic, Show)
  deriving anyclass (Csv.FromRecord, Csv.ToRecord)

{- | Gets all orders that have been placed for a specific token sale and that are still open
and optionally dumps them to a file.
-}
ordersIO
  :: GYNetworkId
  -- ^ The network identifier.
  -> GYProviders
  -- ^ The providers.
  -> GYCompiledScripts
  -- ^ Record of compiled scripts.
  -> TokenSaleParams
  -- ^ Token sale parameters.
  -> Maybe FilePath
  -- ^ Optional CSV-file to write the orders to.
  -> IO (Map GYTxOutRef OrderInfo)
  -- ^ Returns the open orders.
ordersIO nid providers gycs tsp mfile = do
  m <- runGYTxQueryMonadIO nid providers $ runReaderT (orders tsp) gycs
  gyLogInfo providers "" $ printf "found %d open orders" $ Map.size m
  case mfile of
    Just file -> do
      LBS.writeFile file
        $ Csv.encodeWith (Csv.defaultEncodeOptions {Csv.encUseCrLf = False})
        $ Map.elems m
      gyLogInfo providers "" $ printf "dumped open orders to %s" file
    Nothing -> return ()
  return m

-- | Calculates the token distribution for a token sale.
calculateDistributionIO
  :: GYNetworkId
  -- ^ The network identifier.
  -> GYProviders
  -- ^ The providers.
  -> GYCompiledScripts
  -- ^ Record of compiled scripts.
  -> TokenSaleParams
  -- ^ Token sale parameters.
  -> Natural
  -- ^ Total token supply.
  -> Natural
  -- ^ Maximal token allocation.
  -> Maybe FilePath
  -- ^ Optional CSV-file to dump the open orders to.
  -> FilePath
  -- ^ File containing whitelisted addresses.
  -> FilePath
  -- ^ CSV-file to write the distribution to.
  -> Maybe ApiToken
  -- ^ ChainAnalysis Api Token
  -> IO ()
calculateDistributionIO nid providers gycs tsp supply maxAllocation mOrdersFile whiteListFile distFile cTkn = do
  wh <- fmap (Map.fromList . toList) . Csv.decode Csv.NoHeader <$> LBS.readFile whiteListFile

  m <- ordersIO nid providers gycs tsp mOrdersFile

  case wh of
    Left err -> gyLogError providers "" $ printf "error reading utxo with whitelist: %s" err
    Right whitelist -> do
      let filteredOrders = filterOrders whitelist m
      let verifiedStakeHash = Map.elems whitelist

      kycVerifiedAddress <- filterRiskAddr cTkn $ filterAddr (Map.elems filteredOrders) (Set.fromList $ toList verifiedStakeHash)
      let dist = calculateDistribution tsp supply maxAllocation (Set.fromList kycVerifiedAddress) filteredOrders

      LBS.writeFile distFile
        $ Csv.encodeWith (Csv.defaultEncodeOptions {Csv.encUseCrLf = False})
        $ Map.toList dist
      gyLogInfo providers "" $ printf "wrote distribution with %d assignments to %s" (Map.size dist) distFile
  where
    filterOrders :: Map GYTxOutRef String -> Map GYTxOutRef OrderInfo -> Map GYTxOutRef OrderInfo
    filterOrders whUtxos orderUtxos = Map.restrictKeys orderUtxos $ Map.keysSet whUtxos

    filterAddr :: [OrderInfo] -> Set String -> [GYAddress]
    filterAddr orderInfos sKeys = foldl' (\acc o -> if hasStakeKey (oiOwnerAddr o) sKeys then oiOwnerAddr o : acc else acc) [] orderInfos

    hasStakeKey :: GYAddress -> Set String -> Bool
    hasStakeKey addr sKeys = case stakeKeyFromAddress addr of
      Just key -> key `Set.member` sKeys
      _ -> False

    filterRiskAddr :: Maybe ApiToken -> [GYAddress] -> IO [GYAddress]
    filterRiskAddr Nothing xs = pure xs
    filterRiskAddr (Just token) xs = foldM go [] xs
      where
        go :: [GYAddress] -> GYAddress -> IO [GYAddress]
        go acc addr = do
          resp <- registerAndAnalyze token $ addressToBech32 addr
          if risk resp < High
            then return $ addr : acc
            else return acc

-- | Distributes the tokens in a token sale.
distributeIO
  :: ToShelleyWitnessSigningKey k
  => GYNetworkId
  -- ^ The network identifier.
  -> GYProviders
  -- ^ The providers.
  -> GYCompiledScripts
  -- ^ Record of compiled scripts.
  -> GYAddress
  -- ^ The token seller's address.
  -> GYTxOutRef
  -- ^ The token seller's collateral.
  -> k
  -- ^ The token seller's payment signing key.
  -> Maybe GYAddress
  -- ^ Address to receive the payment; if 'Nothing', payment will go to the distributor.
  -> TokenSaleParams
  -- ^ Token sale parameters.
  -> FilePath
  -- ^ CSV-file containing the distribution.
  -> IO ()
distributeIO nid providers gycs addr collateral skey mPaymentAddr tsp distFile = do
  e <- fmap (Map.fromList . toList) . Csv.decode Csv.NoHeader <$> LBS.readFile distFile
  case e of
    Left err -> gyLogError providers "" $ printf "error reading distribution: %s" err
    Right dist -> do
      refs <- Map.filter (\OrderInfo {..} -> Map.member oiRef dist) <$> ordersIO nid providers gycs tsp Nothing
      d <- distWithOrders dist refs
      go (Map.toList d)
  where
    go :: [(GYTxOutRef, (Maybe OrderInfo, Natural))] -> IO ()
    go [] = gyLogInfo providers "" "completed distribution"
    go refs = do
      let
        (xs, ys) = splitAt 10 refs
        refMap = Map.fromList xs
      txBodyFill <- runGYTxMonadNode nid providers [addr] addr (Just (collateral, True)) $ runReaderT (fillOrders tsp refMap mPaymentAddr) gycs
      gyLogInfo providers "" $ printf "filled orders: %s" $ show $ Map.keys refMap
      void $ gySubmitTx providers $ signGYTxBody txBodyFill [skey]

      go ys

distributeMultiTxIO
  :: ToShelleyWitnessSigningKey k
  => GYNetworkId
  -- ^ The network identifier.
  -> GYProviders
  -- ^ The providers.
  -> GYCompiledScripts
  -- ^ Record of compiled scripts.
  -> [GYAddress]
  -- ^ The token seller's addresses.
  -> GYTxOutRef
  -- ^ The token seller's collateral.
  -> k
  -- ^ The token seller's payment signing key.
  -> Maybe GYAddress
  -- ^ Address to receive the payment; if 'Nothing', payment will go to the distributor.
  -> TokenSaleParams
  -- ^ Token sale parameters.
  -> FilePath
  -- ^ CSV-file containing the distribution.
  -> ([TsDistributionLog] -> IO ())
  -- ^ function to write summary log
  -> IO ()
distributeMultiTxIO nid providers gycs addrs collateral skey mPaymentAddr tsp distFile summaryHandler = do
  e <- fmap (Map.fromList . toList) . Csv.decode Csv.NoHeader <$> LBS.readFile distFile
  tAddr <- runGYTxQueryMonadIO nid providers $ runReaderT (orderAddress tsp) gycs

  case e of
    Left err -> gyLogError providers "" $ printf "error reading distribution: %s" err
    Right dist -> do
      gyLogInfo providers "" $ printf "fetching Utxo with Datum"
      refs <- Map.filter (\OrderInfo {..} -> Map.member oiRef dist) <$> ordersIO nid providers gycs tsp Nothing
      gyLogInfo providers "" $ printf "fetched Utxo refs"
      d <- distWithOrders dist refs
      gyLogInfo providers "" $ printf "total distrubution: %d" $ Map.size d
      submitOrders tAddr 3 (Map.toList d)
  where
    -- Todo(piyush): fetch delay in better way
    delaySec =
      case nid of
        GYPrivnet _ -> 5_000_000
        _ -> 100_000_000

    submitOrders :: GYAddress -> Int -> [(GYTxOutRef, (Maybe OrderInfo, Natural))] -> IO ()
    submitOrders _ _ [] = gyLogInfo providers "" $ printf "completed all distribution"
    submitOrders _ 0 refs = gyLogError providers "" $ printf "failed to submit transaction for %d  skeletons" $ show $ length refs
    submitOrders sAddr retryCnt refs = do
      let
        refChunks = LS.chunksOf 10 refs
        toFill = take 5 refChunks
        nextFill = concat $ drop 5 refChunks

      txBodyFill <-
        runGYTxMonadNodeParallel nid providers addrs (head addrs) (Just (collateral, True))
          $ traverse (\r -> runReaderT (fillOrders tsp (Map.fromList r) mPaymentAddr) gycs) toFill
      gyLogInfo providers "" $ printf "generated BuildResult"

      case txBodyFill of
        GYTxBuildSuccess bodies -> do
          let lBdy = NE.toList bodies

          txIds <- liftIO $ traverse (\b -> gySubmitTx providers $ signGYTxBody b [skey]) lBdy
          let filledRefs = concat $ take (NE.length bodies) refChunks
          let indexedTx = map (\(i, txid) -> (txid, refChunks !! i)) (withIndexedList txIds)

          gyLogInfo providers "" $ printf "filled orders: %s" $ show $ Map.keys $ Map.fromList filledRefs
          gyLogInfo providers "" $ printf "completed distribution \n %s" (show txIds)
          void $ summaryHandler $ distLogWithRefs sAddr indexedTx
          gyLogInfo providers "" $ printf "Writing Summery"
          gyLogInfo providers "" $ printf "\n Waiting for another tx \n"
          threadDelay delaySec -- wait to make sure tx available on chain
          submitOrders sAddr 3 nextFill
        GYTxBuildPartialSuccess _ bodies -> do
          let lBdy = NE.toList bodies

          txIds <- liftIO $ traverse (\b -> gySubmitTx providers $ signGYTxBody b [skey]) lBdy
          let
            filledRefs = concat $ take (NE.length bodies) refChunks
            indexedTx = map (\(i, txid) -> (txid, refChunks !! i)) (withIndexedList txIds)

          gyLogInfo providers "" $ printf "filled orders: %s" $ show $ Map.keys $ Map.fromList filledRefs
          gyLogInfo providers "" $ printf "partial distribution \n %s \n" (show txIds)

          void $ summaryHandler $ distLogWithRefs sAddr indexedTx
          gyLogInfo providers "" $ printf "Writing Summery"

          gyLogInfo providers "" $ printf "\n Waiting  for another tx \n"
          threadDelay delaySec -- wait to make sure tx available on chain
          submitOrders sAddr 3 $ concat $ drop (NE.length bodies) refChunks
        GYTxBuildFailure val -> do
          gyLogInfo providers "" $ printf "fail to fill order for value: %s" $ show val
          threadDelay delaySec -- wait to make sure tx available on chain
          submitOrders sAddr (retryCnt - 1) refs
        GYTxBuildNoInputs -> gyLogError providers "" "Error: No Build Inputs"

    withIndexedList :: [a] -> [(Int, a)]
    withIndexedList = zip [0 ..]

    distLogWithRefs :: GYAddress -> [(GYTxId, [(GYTxOutRef, (Maybe OrderInfo, Natural))])] -> [TsDistributionLog]
    distLogWithRefs sAddr = concatMap refToDist
      where
        refToDist :: (GYTxId, [(GYTxOutRef, (Maybe OrderInfo, Natural))]) -> [TsDistributionLog]
        refToDist (txId, refs) = foldr (refToDistLog txId) [] refs

        refToDistLog :: GYTxId -> (GYTxOutRef, (Maybe OrderInfo, Natural)) -> [TsDistributionLog] -> [TsDistributionLog]
        refToDistLog _ (_, (Nothing, _)) dLogs = dLogs
        refToDistLog txId (ref, (Just o, amt)) dLogs =
          let payAddr = oiOwnerAddr o
          in TsDistributionLog {tdRoundAddr = sAddr, tdOrderRef = ref, tdAmt = amt, tdPayAddr = payAddr, tdStakeKey = stakeKey payAddr, tdTxId = show txId} : dLogs

        stakeKey addr = fromJust $ stakeKeyFromAddress addr

distWithOrders :: Map GYTxOutRef Natural -> Map GYTxOutRef OrderInfo -> IO (Map GYTxOutRef (Maybe OrderInfo, Natural))
distWithOrders dist refs = do
  return $ Map.filter (isJust . fst) $ Map.mapWithKey (\k v -> (Map.lookup k refs, v)) dist

data TsSummaryInfo = TsSummaryInfo
  { siTotalDistributed :: Int
  , siRemainingUtxoToDistribute :: [GYTxOutRef]
  , siRemainingUtxoInChain :: [GYTxOutRef]
  }
  deriving (Aeson.FromJSON, Aeson.ToJSON, Generic, Show)

tokenSaleSummaryIO
  :: GYNetworkId
  -- ^ The network identifier
  -> GYProviders
  -- ^ Providers
  -> GYCompiledScripts
  -- ^ Record of compiled scripts.
  -> TokenSaleParams
  -- ^ Tokensale parameters
  -> FilePath
  -- ^ Distribution file  path
  -> FilePath
  -- ^ path to store summary of distribution
  -> IO ()
tokenSaleSummaryIO nid providers gycs tsParams distFile summaryFile = do
  e <- fmap (Map.fromList . toList) . Csv.decode Csv.NoHeader <$> LBS.readFile distFile
  case e of
    Left err -> gyLogError providers "" $ printf "error reading distribution: %s" err
    Right dist -> do
      refs <- ordersIO nid providers gycs tsParams Nothing
      let
        dLeft = distributionLeft dist refs
        tot = Map.size dist - length dLeft
        summary = TsSummaryInfo tot dLeft (utxoLeft dist refs)
      LBS.writeFile summaryFile $ Aeson.encode summary
      gyLogInfo providers "" $ printf "wrote distribution with %d assignments to %s" (Map.size dist) distFile
      pure ()
  where
    distributionLeft :: Map GYTxOutRef Natural -> Map GYTxOutRef OrderInfo -> [GYTxOutRef]
    distributionLeft dist oUtxos = Map.keys $ Map.intersection dist oUtxos

    utxoLeft :: Map GYTxOutRef Natural -> Map GYTxOutRef OrderInfo -> [GYTxOutRef]
    utxoLeft dist oUtxos = Map.keys $ Map.difference oUtxos dist

simTokenSaleWhitelistIO
  :: GYNetworkId
  -> GYProviders
  -> GYCompiledScripts
  -> TokenSaleParams
  -> IO [WhiteListInfo]
simTokenSaleWhitelistIO nid providers gycs tsParams = do
  m <- ordersIO nid providers gycs tsParams Nothing
  pure $ map toWhitelistInfo $ Map.toList m
  where
    toWhitelistInfo :: (GYTxOutRef, OrderInfo) -> WhiteListInfo
    toWhitelistInfo (ref, ordr) = WhiteListInfo ref $ fromMaybe "" (stakeKeyFromAddress $ oiOwnerAddr ordr)

data DbWhitelistResp = DbWhitelistResp
  {dbWhTx :: !String, dbWhTxId :: !Int, dbWhStakeKeyHash :: !String}
  deriving (FromRow, Generic, Show)

dbTokenSaleWhitelistIO
  :: GYNetworkId
  -- ^ Network ID
  -> GYProviders
  -- ^ The Providers
  -> String
  -- ^ Database Url
  -> Natural
  -- ^ Total Supply for TokenSale
  -> Natural
  -- ^ Max Allocation for TokenSale
  -> GYCompiledScripts
  -- ^ Compiled Script
  -> TokenSaleParams
  -- ^ TokenSale Params
  -> IO [WhiteListInfo]
dbTokenSaleWhitelistIO nid providers dbUrl tsSupply tsMaxAlloc gycs tsParam = do
  scriptAddr <- runGYTxQueryMonadIO nid providers $ runReaderT (orderAddress tsParam) gycs
  conn <- connectPostgreSQL $ encodeUtf8 $ T.pack dbUrl
  roundInfo <- roundDetail conn (addressToText scriptAddr)
  void $ validateTokenSaleConfig roundInfo
  res :: [DbWhitelistResp] <- query conn whitelistSqlQuery [addressToText scriptAddr]
  pure $ map parseResult res
  where
    parseResult :: DbWhitelistResp -> WhiteListInfo
    parseResult DbWhitelistResp {..} =
      let ref = dbWhTx <> "#" <> show dbWhTxId
      in WhiteListInfo (fromString ref) dbWhStakeKeyHash

    validateTokenSaleConfig :: TokenRoundInfo -> IO ()
    validateTokenSaleConfig TokenRoundInfo {..}
      | toInteger tsMaxAlloc /= reMaxAllocation = fail $ "invalid max allocation. Round has:" <> show reMaxAllocation <> " but supplied in params: " <> show tsMaxAlloc
      | toInteger tsSupply /= reTotalSupply = fail $ "invalid Total supply. Round has: " <> show reTotalSupply <> " but supplied in params " <> show tsSupply
      | otherwise = return ()
