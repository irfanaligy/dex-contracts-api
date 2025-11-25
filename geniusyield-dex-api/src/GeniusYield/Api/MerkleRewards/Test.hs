module GeniusYield.Api.MerkleRewards.Test
  ( rc
  , src
  , src66
  , src666
  , src6666
  , sref
  , url_100_10
  , url_1000_10
  , url_10000_10
  , url_100_25
  , url_1000_25
  , url_10000_25
  , url_100_50
  , url_1000_50
  , url_10000_50
  , preparePlacement
  , preparePlacementForRewardees
  , rewardeeAddr
  , allRewardsForRewardeeIO'
  )
where

import Control.Monad (replicateM)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import GeniusYield.Imports
import GeniusYield.TxBuilder (mustMint)
import GeniusYield.Types
import System.Random

import GeniusYield.Api.MerkleRewards
import GeniusYield.OnChain.AnyMint.Compiled (originalAnyMintPolicy)

rc :: RewardsConfig
rc =
  RewardsConfig
    { rcCfgFile = "/home/brunjlar/code/geniusyield/core/config-core-maestro-preprod.json"
    , rcOwner = "062a6d3a93db055290620a139091da9c7858fbcb3310c58564687c3f"
    , rcReconstructor = reconstructorIO
    }

src, src66, src666, src6666 :: SubmitRewardsConfig
src =
  SubmitRewardsConfig
    { srcCfg = rc
    , srcSKeyFile = "/home/brunjlar/code/geniusyield/gens/rewards/rewards-owner.skey"
    , srcCollateral = "082581ecf2b10429573f0e0d5a005a2f5f33947afd18436639258ce6e37f8e91#0"
    }
src66 =
  SubmitRewardsConfig
    { srcCfg = rc
    , srcSKeyFile = "/home/brunjlar/code/geniusyield/core/rewards-tests/rewardees-100/user-00066.skey"
    , srcCollateral = "0312c6a724cd436d74b88a3bf55dd93ade60cc60a40ee7f9577c0a8c52b19b90#0"
    }
src666 =
  SubmitRewardsConfig
    { srcCfg = rc
    , srcSKeyFile = "/home/brunjlar/code/geniusyield/core/rewards-tests/rewardees-1000/user-00666.skey"
    , srcCollateral = "56f0d548e7dba3fabe31af62113b7a57045bb0070a45b4263641cfb0f56eccd8#0"
    }
src6666 =
  SubmitRewardsConfig
    { srcCfg = rc
    , srcSKeyFile = "/home/brunjlar/code/geniusyield/core/rewards-tests/rewardees-10000/user-06666.skey"
    , srcCollateral = "0a889d9035a405edeb3cd57d96577dd35444df7b5644c9f959d541a17dbc527d#0"
    }

sref :: GYTxOutRef
sref = "db51b5f5cc2c6067023847704b4b2276fbc49c44dbe414c28b15add5fc6a24dd#0"

url_100_10, url_1000_10, url_10000_10, url_100_25, url_1000_25, url_10000_25, url_100_50, url_1000_50, url_10000_50 :: String
url_100_10 = "https://gist.githubusercontent.com/brunjlar/cf8e7e8ec1a72c8c50a9c10af8ee1ce0/raw/d9b0907fd12d67a85c339614b2122fa7ade99ba3/rewards-100.json"
url_1000_10 = "https://gist.githubusercontent.com/brunjlar/c2f9ee493ad69f6f2d96d25230da171e/raw/618ebdd23ff4ca6b89d1139f266174231a3bfed1/rewards-1000.json"
url_10000_10 = "https://gist.githubusercontent.com/brunjlar/00179acad435def7689c213ad5ce148b/raw/5b006e080334ee7783ee8a044ddd2da2f521d75b/rewards-10000.json"
url_100_25 = "https://gist.githubusercontent.com/brunjlar/7f55d22deea2828907bbe85e5887b0bf/raw/220c122cfc9ff0fa846b760158cc173d97095ca7/rewards-100_25.json"
url_1000_25 = "https://gist.githubusercontent.com/brunjlar/078e9949dd52fdab5b28ff8298ced483/raw/a4e913d18508b86ea60745b0f04851e23f39b13d/rewards-1000_25.json"
url_10000_25 = "https://gist.githubusercontent.com/brunjlar/23208ca428ae24116db8cf53f59e9f7b/raw/fca7cb21be8f0d5dd3b72d7588b7ae8ddc7a48dc/rewards-10000_25.json"
url_100_50 = "https://gist.githubusercontent.com/brunjlar/cf8e7e8ec1a72c8c50a9c10af8ee1ce0/raw/a3670afd5da34836cd2ffacf13dcdfc89c0de5eb/rewards-100.json"
url_1000_50 = "https://gist.githubusercontent.com/brunjlar/c2f9ee493ad69f6f2d96d25230da171e/raw/efb3d5f4db5c1731012af89a57205f74fbd77447/rewards-1000.json"
url_10000_50 = "https://gist.githubusercontent.com/brunjlar/00179acad435def7689c213ad5ce148b/raw/eb036b3f597f66787c791ce76ced4dfd5d6ef2dc/rewards-10000.json"

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

mintTestTokens
  :: SubmitRewardsConfig
  -> Map GYTokenName Natural
  -> IO GYTxId
mintTestTokens cfg m = submitRewardsIO cfg $ \_ _ _ _ -> do
  let
    xs = Map.toList m
    ys = [mustMint (GYMintScript anyMintPolicy) unitRedeemer tn $ toInteger n | (tn, n) <- xs]
  pure $ mconcat ys

preparePlacement'
  :: SubmitRewardsConfig
  -> [GYPaymentSigningKey]
  -> Int
  -- ^ Number of tokens.
  -> FilePath
  -- ^ Rewards file.
  -> IO ()
preparePlacement' cfg skeys tokenCount rewFile = do
  let rewCount = length skeys
  tns <- randomTokenNames tokenCount
  let addrs = [addressFromPaymentKeyHash GYTestnetPreprod (paymentKeyHash $ paymentVerificationKey skey) | skey <- skeys]
  rs <- replicateM rewCount $ reward tns
  let totals = Map.unionsWith (+) rs
  void $ mintTestTokens cfg totals
  let rs' = toVal <$> rs
  writeRewards rewFile $ zip addrs rs'
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
  :: SubmitRewardsConfig
  -> Int
  -- ^ Number of rewardees.
  -> Int
  -- ^ Number of tokens.
  -> FilePath
  -- ^ Folder for users.
  -> FilePath
  -- ^ Rewards file.
  -> IO ()
preparePlacement cfg rewCount tokenCount rewFolder rewFile = do
  skeys <- generateUsers rewCount rewFolder
  preparePlacement' cfg skeys tokenCount rewFile

preparePlacementForRewardees
  :: SubmitRewardsConfig
  -> Int
  -- ^ Number of rewardees.
  -> Int
  -- ^ Number of tokens.
  -> FilePath
  -- ^ Folder for users.
  -> FilePath
  -- ^ Rewards file.
  -> IO ()
preparePlacementForRewardees cfg rewCount tokenCount rewFolder rewFile = do
  skeys <- forM [1 .. rewCount] $ \i -> readPaymentSigningKey $ rewardeeFileName rewFolder i
  preparePlacement' cfg skeys tokenCount rewFile

rewardeeAddr :: FilePath -> Int -> IO GYAddress
rewardeeAddr folder i = do
  skey <- readPaymentSigningKey $ rewardeeFileName folder i
  pure $ addressFromPaymentKeyHash GYTestnetPreprod $ paymentKeyHash $ paymentVerificationKey skey

allRewardsForRewardeeIO' :: FilePath -> Int -> IO ()
allRewardsForRewardeeIO' folder i = do
  addr <- rewardeeAddr folder i
  allRewardsForRewardeeIO rc $ fromJust $ rewardeeFromAddress addr
