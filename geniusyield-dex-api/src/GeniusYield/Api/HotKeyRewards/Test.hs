module GeniusYield.Api.HotKeyRewards.Test
  ( cfgIO
  , rcIO
  , crcIO
  , agentAddr
  , chris1Addr
  , chris2Addr
  , larsEternlAddr
  , larsNami1Addr
  , larsNami2Addr
  , acAgent
  , skeyFileAgent
  , preparePlacement
  , preparePlacementForRewardees
  , rewardeeAddr
  , allRewardsForRewardeeIO'
  , withdrawAllRewardsIO'
  , lovelace
  , wolpertinger
  , hubert
  , tAgix
  , tHosky
  , tMeld
  , tGens
  , rsChris
  , rsLars
  )
where

import Control.Monad (replicateM)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import GeniusYield.GYConfig (GYCoreConfig, coreConfigIO, withCfgProviders)
import GeniusYield.Imports
import GeniusYield.TxBuilder (mustMint)
import GeniusYield.Types
import System.Random

import GeniusYield.Api.HotKeyRewards
import GeniusYield.Api.Types (Secret (..))
import GeniusYield.OnChain.AnyMint.Compiled (originalAnyMintPolicy)
import GeniusYield.Scripts (readCompiledScripts)

cfgIO :: IO GYCoreConfig
cfgIO = coreConfigIO "/home/brunjlar/code/geniusyield/core/config-core-maestro-preprod.json"

rcIO :: IO RewardsConfig
rcIO = do
  cfg <- cfgIO
  gycs <- readCompiledScripts
  pure
    RewardsConfig
      { rcCfg = cfg
      , rcOwner = "062a6d3a93db055290620a139091da9c7858fbcb3310c58564687c3f"
      , rcReconstructor = reconstructorIO
      , rcCache = Nothing
      , rcCompiledScripts = gycs
      }

crcIO :: IO ConstructRewardsConfig
crcIO = do
  cfg <- cfgIO
  skey <- readPaymentSigningKey "/home/brunjlar/code/geniusyield/gens/rewards/rewards-owner.skey"
  gycs <- readCompiledScripts
  pure
    ConstructRewardsConfig
      { crcCfg = cfg
      , crcOwnerSKey = Secret skey
      , crcReconstructor = reconstructorIO
      , crcCache = Nothing
      , crcCompiledScripts = gycs
      }

agentAddr, chris1Addr, chris2Addr, larsEternlAddr, larsNami1Addr, larsNami2Addr :: GYAddress
agentAddr = unsafeAddressFromText "addr_test1vrgg5hurpxmde5wez2chxzdu73fnt5guql4wrv4g2zwylggq655pq"
chris1Addr = unsafeAddressFromText "addr_test1qqj3jkhct3qmnkta5l60y9wnuaxfemmlq3ee66aywwa89g343f8yzpwq3av7gauswp5ec7nj2e5fxve0nptaknn5904sj9ysa3"
chris2Addr = unsafeAddressFromText "addr_test1qqqswr0dl3ekeeuu2jzhupd74lzpz7mhtwvrhra8gm553cu06h6aqwr8qd3zch7syng5gl2e2r8d4v0ewgcns53a78rqgu9c3l"
larsEternlAddr = unsafeAddressFromText "addr_test1qpyzck9j365a5cck7856maaumme4hhg26pyrz8mg7qj5pjdaavghhj8e4rryf0xyth5yj0yu7lcxulk6rqhwfvel7p0qz5jvx9"
larsNami1Addr = unsafeAddressFromText "addr_test1qpf7q458ezx0z3qmgg6lrrfpvmruckawl8hq04ymf4gz9cmqywjfk4xdcxhykzzfmklgzjrjj3xlfz5q9k2zc9tc39ps8mtwhv"
larsNami2Addr = unsafeAddressFromText "addr_test1qp2g0mjqthqd37v9nqhphhph8jdxemst760xls6rd9fs5mj74qys60j3rkz63s7ke9zvhafzhvnz42mw8cvu4cx0qczqvhyfm5"

acAgent :: ActorConfig
acAgent = ActorConfig [agentAddr] agentAddr (Just "f4885720bd553fe917ea36f4168d959da9f4debf9d2f43be460b7bb0b3ee8253#0")

skeyFileAgent :: FilePath
skeyFileAgent = "skeys/agent.skey"

rewardeeFileName :: FilePath -> Int -> FilePath
rewardeeFileName = printf "%s/user-%05d.skey"

generateUsers :: Int -> FilePath -> IO [GYPaymentSigningKey]
generateUsers n folder = forM [1 .. n] $ \i -> do
  skey <- generatePaymentSigningKey
  writePaymentSigningKey (rewardeeFileName folder i) skey
  pure skey

randomTokenNames :: Int -> IO [GYTokenName]
randomTokenNames n = go mempty
  where
    go :: Set GYTokenName -> IO [GYTokenName]
    go s
      | Set.size s == n = pure $ Set.toList s
      | otherwise = do
          tn <- randomTokenName
          go $ Set.insert tn s

    randomTokenName :: IO GYTokenName
    randomTokenName = do
      l <- randomRIO (1, 32)
      s <- replicateM l randomLetter
      pure $ fromString s

    randomLetter :: IO Char
    randomLetter = do
      i <- randomRIO (0, 25)
      pure $ ['A' .. 'Z'] !! i

anyMintPolicy :: GYScript PlutusV2
anyMintPolicy = mintingPolicyFromPlutus originalAnyMintPolicy

anyMintId :: GYMintingPolicyId
anyMintId = mintingPolicyId anyMintPolicy

mintTestTokens :: Map GYTokenName Natural -> IO GYTxId
mintTestTokens m = do
  skey <- readPaymentSigningKey skeyFileAgent
  crc <- crcIO
  constructRewardsIO
    crc
    acAgent
    ( \_gycs _reconstructor _pkh -> do
        let
          xs = Map.toList m
          ys = [mustMint (GYMintScript anyMintPolicy) unitRedeemer tn $ toInteger n | (tn, n) <- xs]
        pure $ mconcat ys
    )
    (signAndSubmit skey)

preparePlacement'
  :: [GYPaymentSigningKey]
  -> Int
  -- ^ Number of tokens.
  -> FilePath
  -- ^ Rewards file.
  -> IO ()
preparePlacement' skeys tokenCount rewFile = do
  let rewCount = length skeys
  tns <- randomTokenNames tokenCount
  let addrs = [addressFromPaymentKeyHash GYTestnetPreprod (paymentKeyHash $ paymentVerificationKey skey) | skey <- skeys]
  rs <- replicateM rewCount $ reward tns
  let totals = Map.unionsWith (+) rs
  void $ mintTestTokens totals
  let rs' = toVal <$> rs
  writeRewards' rewFile $ zip addrs rs'
  where
    reward :: [GYTokenName] -> IO (Map GYTokenName Natural)
    reward tns = do
      xs <- forM tns $ \tn -> do
        n <- fromInteger <$> randomRIO (0, 1_000_000_000_000)
        pure (tn, n)
      pure $ Map.fromList xs

    toVal :: Map GYTokenName Natural -> GYValue
    toVal = Map.foldMapWithKey (\tn n -> valueSingleton (GYToken anyMintId tn) $ toInteger n)

preparePlacement
  :: Int
  -- ^ Number of rewardees.
  -> Int
  -- ^ Number of tokens.
  -> FilePath
  -- ^ Folder for users.
  -> FilePath
  -- ^ Rewards file.
  -> IO ()
preparePlacement rewCount tokenCount rewFolder rewFile = do
  skeys <- generateUsers rewCount rewFolder
  preparePlacement' skeys tokenCount rewFile

preparePlacementForRewardees
  :: Int
  -- ^ Number of rewardees.
  -> Int
  -- ^ Number of tokens.
  -> FilePath
  -- ^ Folder for users.
  -> FilePath
  -- ^ Rewards file.
  -> IO ()
preparePlacementForRewardees rewCount tokenCount rewFolder rewFile = do
  skeys <- forM [1 .. rewCount] $ \i -> readPaymentSigningKey $ rewardeeFileName rewFolder i
  preparePlacement' skeys tokenCount rewFile

rewardeeAddr :: FilePath -> Int -> IO GYAddress
rewardeeAddr folder i = do
  skey <- readPaymentSigningKey $ rewardeeFileName folder i
  pure $ addressFromPaymentKeyHash GYTestnetPreprod $ paymentKeyHash $ paymentVerificationKey skey

allRewardsForRewardeeIO' :: FilePath -> Int -> IO ()
allRewardsForRewardeeIO' folder i = do
  rc <- rcIO
  addr <- rewardeeAddr folder i
  void $ allRewardsForRewardeeIO rc $ fromJust $ rewardeeFromAddress addr

withdrawAllRewardsIO' :: FilePath -> Int -> IO ()
withdrawAllRewardsIO' folder i = do
  skey <- readPaymentSigningKey $ rewardeeFileName folder i
  addr <- rewardeeAddr folder i
  let
    ac = ActorConfig [addr] addr Nothing
    pkh = fromJust $ addressToPubKeyHash addr
  crc <- crcIO
  tx <- withdrawAllRewardsIO crc ac $ BotRewardee pkh
  withCfgProviders (crcCfg crc) "rewards-tests" $ \providers ->
    void $ signAndSubmit skey providers undefined tx

wolpertingerAC, hubertAC, tAgixAC, tHoskyAC, tMeldAC, tGensAC :: GYAssetClass
wolpertingerAC =
  fromRight (error "invalid asset class")
    $ parseAssetClassWithSep
      '.'
      "2befced6786559558bcf4b89655d564f2d37b5222db1e5b8def9a2e9.576f6c70657274696e676572"
hubertAC =
  fromRight (error "invalid asset class")
    $ parseAssetClassWithSep
      '.'
      "514ae1c9aa96540f2a6bafb971be8392eec23e4c7cd7473f2bbd5937.487562657274"
tAgixAC =
  fromRight (error "invalid asset class")
    $ parseAssetClassWithSep
      '.'
      "1d32893c51b2a88dc034c726a7f1f765539ea739f08950e4fb31e36b.7441474958"
tHoskyAC =
  fromRight (error "invalid asset class")
    $ parseAssetClassWithSep
      '.'
      "c8dc7113db55c16195f19187030585f0ec24ab3a64722a3627b3a8f1.74484f534b59"
tMeldAC =
  fromRight (error "invalid asset class")
    $ parseAssetClassWithSep
      '.'
      "66a524d7f34d954a3ad30b4e2d08023c950dfcd53bbe3c2314995da6.744d454c44"
tGensAC =
  fromRight (error "invalid asset class")
    $ parseAssetClassWithSep
      '.'
      "c6e65ba7878b2f8ea0ad39287d3e2fd256dc5c4160fc19bdf4c4d87e.7447454e53"

lovelace, wolpertinger, hubert, tAgix, tHosky, tMeld, tGens :: Natural -> GYValue
lovelace = valueSingleton GYLovelace . toInteger
wolpertinger = valueSingleton wolpertingerAC . toInteger
hubert = valueSingleton hubertAC . toInteger
tAgix = valueSingleton tAgixAC . toInteger
tHosky = valueSingleton tHoskyAC . toInteger
tMeld = valueSingleton tMeldAC . toInteger
tGens = valueSingleton tGensAC . toInteger

rsChris, rsLars :: Rewards
rsChris =
  toRewards
    [ (chris1Addr, lovelace 250_000_000 <> tGens 1_000_000_000 <> tAgix 1_000_000_000)
    , (chris2Addr, lovelace 750_000_000 <> tMeld 50_000_000 <> tHosky 50)
    ]
rsLars =
  toRewards
    [ (larsEternlAddr, lovelace 250_000_000 <> tGens 600_000_000 <> tAgix 1_000_000_000)
    , (larsNami1Addr, lovelace 750_000_000 <> tMeld 5_000_000 <> tHosky 10)
    ]
