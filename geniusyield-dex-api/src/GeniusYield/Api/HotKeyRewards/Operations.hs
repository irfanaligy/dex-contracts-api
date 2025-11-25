{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

module GeniusYield.Api.HotKeyRewards.Operations
  ( rewardsAddress
  , allRewardsInfos
  , rewardsInfo
  , placeRewards
  , topUpRewards
  , clawBackRewards
  , declutterRewards
  , mkReconstructor
  , insertRewardsInOptionalCache
  , calculateRewards
  , rewardsForRewardee
  , allRewardsForRewardee
  , withdrawRewards
  , withdrawAllRewards
  , rewardsInfoToDatum
  , rewardeeFromAddress
  , toRewards
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (ask))
import Data.Foldable (Foldable (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GeniusYield.Imports
import GeniusYield.TxBuilder
  ( GYTxQueryMonad (logMsg, networkId, utxosAtPaymentCredential, utxosAtPaymentCredentialWithDatums)
  , GYTxSkeleton
  , GYTxUserQueryMonad
  , datumHashFromPlutus'
  , gyLogInfo'
  , lookupDatum'
  , mustBeSignedBy
  , mustHaveInput
  , mustHaveOutput
  , mustHaveTxMetadata
  , mustMint
  , pubKeyHashFromPlutus'
  , someUTxOWithoutRefScript
  , throwAppError
  , tokenNameFromPlutus'
  , utxoAtTxOutRefWithDatum'
  , utxosDatumsPure
  )
import GeniusYield.Types
import PlutusLedgerApi.V1 (UnsafeFromData (..))
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import Unsafe.Coerce (unsafeCoerce)

import GeniusYield.Api.Cache (Cache (cGet), cPut')
import GeniusYield.Api.HotKeyRewards.Types
  ( GYRewardsException (..)
  , LastRewardsAction (..)
  , Reconstructor
  , Rewardee (..)
  , Rewards (..)
  , RewardsDatum (..)
  , RewardsInfo (..)
  , RewardsStep (..)
  )
import GeniusYield.Api.Utils (stampRewardsClaimed)
import GeniusYield.Scripts (GYCompiledScripts (signedMintPolicy))
import GeniusYield.Scripts.DEX (expectedTokenName)
import GeniusYield.Scripts.MerkleRewards.Hashable (Hashable (..))

-- Do we want cache as required or optional? For now it is kept optional.
type HotKeyQueryMonad m = (HasCallStack, MonadReader (Maybe (Cache IO GYDatumHash RewardsStep)) m, GYTxQueryMonad m, MonadIO m)

type HotKeyMonad m = (HotKeyQueryMonad m, GYTxUserQueryMonad m)

rewardsAddress
  :: (GYTxQueryMonad m, HasCallStack)
  => GYPubKeyHash
  -- ^ The pubkey hash of the owner.
  -> Maybe GYStakeKeyHash
  -- ^ Optional stake key hash to use.
  -> m GYAddress
rewardsAddress pkh mskh = do
  nid <- networkId
  let pc = GYCredentialByKey (fromPubKeyHash pkh)
  pure
    $ addressFromCredential
      nid
      pc
      (GYCredentialByKey <$> mskh)

rewardsInfos
  :: forall m
   . (GYTxQueryMonad m, HasCallStack)
  => GYCompiledScripts
  -> GYPubKeyHash
  -> [(GYUTxO, Maybe GYDatum)]
  -> m (Map GYTxOutRef RewardsInfo)
rewardsInfos gycs pkh utxosWithDatums = iwither f $ utxosDatumsPure utxosWithDatums
  where
    f :: GYTxOutRef -> (GYAddress, GYValue, RewardsDatum) -> m (Maybe RewardsInfo)
    f ref (addr, value, RewardsDatum {..}) = do
      datumHash <- traverse datumHashFromPlutus' rdPrevious
      nft <- tokenNameFromPlutus' rdNFT

      pure $ do
        let pid = mintingPolicyId $ signedMintPolicy gycs pkh
        guard $ valueAssetClass value (GYToken pid nft) == 1

        let
          BuiltinByteString bs = rdInfo
          einfo = Text.decodeUtf8' bs
        case einfo of
          Left _ -> Nothing
          Right info ->
            pure
              RewardsInfo
                { riInfo = info
                , riRef = ref
                , riAddress = addr
                , riValue = value
                , riPrevious = datumHash
                , riNFT = nft
                , riLastAction = rdLastAction
                , riHash = rdHash
                }

allRewardsInfos :: (GYTxQueryMonad m, HasCallStack) => GYCompiledScripts -> GYPubKeyHash -> m (Map GYTxOutRef RewardsInfo)
allRewardsInfos gycs pkh = do
  utxosWithDatums <- flip utxosAtPaymentCredentialWithDatums Nothing $ GYCredentialByKey $ fromPubKeyHash pkh
  rewardsInfos gycs pkh utxosWithDatums

rewardsInfo :: (GYTxQueryMonad m, HasCallStack) => GYCompiledScripts -> GYPubKeyHash -> GYTxOutRef -> m RewardsInfo
rewardsInfo gycs pkh ref = do
  utxosWithDatums <- pure <$> utxoAtTxOutRefWithDatum' ref
  m <- rewardsInfos gycs pkh utxosWithDatums
  pure $ m Map.! ref

placeRewards
  :: (GYTxUserQueryMonad m, HasCallStack)
  => GYCompiledScripts
  -> GYPubKeyHash
  -- ^ The pubkey hash of the owner.
  -> Maybe GYStakeKeyHash
  -- ^ Optional stake key hash to use.
  -> Text
  -- ^ Information describing these rewards.
  -> String
  -- ^ The URL containing the rewards information.
  -> Rewards
  -- ^ The rewards to place.
  -> m (GYTxSkeleton PlutusV2)
placeRewards gycs pkh mskh info url rs = do
  nftRef <- someUTxOWithoutRefScript
  addr <- rewardsAddress pkh mskh

  let
    nftPolicy = signedMintPolicy gycs pkh
    nftPid = mintingPolicyId nftPolicy
    nftName = expectedTokenName nftRef
    nftToken = GYToken nftPid nftName
    nft = valueSingleton nftToken 1
    value = fold (getRewards rs) <> nft
    h = hash rs
    datum =
      datumFromPlutusData
        RewardsDatum
          { rdInfo = BuiltinByteString $ Text.encodeUtf8 info
          , rdPrevious = Nothing
          , rdNFT = tokenNameToPlutus nftName
          , rdLastAction = RewardsPlaced h $ BuiltinByteString $ Text.encodeUtf8 $ Text.pack url
          , rdHash = h
          }

  pure
    $ mustMint (GYMintScript nftPolicy) unitRedeemer nftName 1
      <> mustBeSignedBy pkh
      <> mustHaveInput
        GYTxIn
          { gyTxInTxOutRef = nftRef
          , gyTxInWitness = GYTxInWitnessKey
          }
      <> mustHaveOutput
        GYTxOut
          { gyTxOutAddress = addr
          , gyTxOutValue = value
          , gyTxOutDatum = Just (datum, GYTxOutDontUseInlineDatum)
          , gyTxOutRefS = Nothing
          }

topUpRewards
  :: HotKeyMonad m
  => GYCompiledScripts
  -> Reconstructor m
  -- ^ A reconstructor.
  -> GYPubKeyHash
  -- ^ The pubkey hash of the owner.
  -> GYTxOutRef
  -- ^ The reference to the rewards to top up.
  -> Text
  -- ^ Information describing these rewards.
  -> String
  -- ^ The URL containing the rewards information.
  -> Rewards
  -- ^ The rewards to place.
  -> m (GYTxSkeleton PlutusV2)
topUpRewards gycs reconstructor pkh ref info url additionalRs = do
  ri@RewardsInfo {..} <- rewardsInfo gycs pkh ref
  let oldDH = hashDatum $ datumFromPlutusData $ rewardsInfoToDatum ri
  oldRs <- calculateRewards reconstructor oldDH

  let
    newRs = oldRs <> additionalRs
    newValue = fold (getRewards additionalRs) <> riValue
    h = hash additionalRs
    datum =
      datumFromPlutusData
        RewardsDatum
          { rdInfo = BuiltinByteString $ Text.encodeUtf8 info
          , rdPrevious = Just $ datumHashToPlutus oldDH
          , rdNFT = tokenNameToPlutus riNFT
          , rdLastAction = RewardsPlaced h $ BuiltinByteString $ Text.encodeUtf8 $ Text.pack url
          , rdHash = hash newRs
          }

  pure
    $ mustHaveInput
      GYTxIn
        { gyTxInTxOutRef = riRef
        , gyTxInWitness = GYTxInWitnessKey
        }
      <> mustHaveOutput
        GYTxOut
          { gyTxOutAddress = riAddress
          , gyTxOutValue = newValue
          , gyTxOutDatum = Just (datum, GYTxOutDontUseInlineDatum)
          , gyTxOutRefS = Nothing
          }

-- | Claw back a specific rewards UTxO.
clawBackRewards
  :: HotKeyMonad m
  => GYCompiledScripts
  -> Reconstructor m
  -- ^ The reconstructor.
  -> GYPubKeyHash
  -- ^ The pubkey hash of the owner.
  -> GYTxOutRef
  -- ^ The reference to the rewards to claw back.
  -> m (Rewards, GYTxSkeleton PlutusV2)
clawBackRewards gycs reconstructor pkh ref = do
  info <- rewardsInfo gycs pkh ref

  let dh = hashDatum $ datumFromPlutusData $ rewardsInfoToDatum info
  rs <- calculateRewards reconstructor dh
  when (hash rs /= riHash info)
    $ throwAppError
    $ GYRewardsMismatch (riHash info) rs

  let skeleton =
        mustMint (GYMintScript $ signedMintPolicy gycs pkh) unitRedeemer (riNFT info) (-1)
          <> mustBeSignedBy pkh
          <> mustHaveInput
            GYTxIn
              { gyTxInTxOutRef = ref
              , gyTxInWitness = GYTxInWitnessKey
              }

  pure (rs, skeleton)

-- | Remove all UTxO's at the rewards address that do not represent rewards.
declutterRewards
  :: (GYTxUserQueryMonad m, HasCallStack)
  => GYCompiledScripts
  -> GYPubKeyHash
  -- ^ The pubkey hash of the owner.
  -> m (GYTxSkeleton PlutusV2)
declutterRewards gycs pkh = do
  infoRefs <- Map.keysSet <$> allRewardsInfos gycs pkh
  allRefs <- Map.keysSet . mapUTxOs (const ()) <$> utxosAtPaymentCredential (GYCredentialByKey $ fromPubKeyHash pkh) Nothing

  let refs = Set.difference allRefs infoRefs

  pure $ foldMap (\ref -> mustHaveInput $ GYTxIn ref GYTxInWitnessKey) refs

mkReconstructor
  :: (GYTxQueryMonad m, HasCallStack)
  => (String -> m Rewards)
  -- ^ Download Rewards from a given URL.
  -> Reconstructor m
mkReconstructor downloadRewards dh = do
  d <- lookupDatum' dh
  let RewardsDatum {..} = unsafeFromBuiltinData $ datumToPlutus' d
  mdh <- traverse datumHashFromPlutus' rdPrevious
  case rdLastAction of
    RewardsPlaced h (BuiltinByteString bs) -> do
      let url = Text.unpack $ decodeUtf8Lenient bs
      logMsg "" GYDebug $ printf "downloading rewards from %s" url
      rs <- downloadRewards url
      when (hash rs /= h)
        $ throwAppError
        $ GYRewardsMismatch h rs
      logMsg "" GYDebug $ printf "rewards downloaded from %s" url
      pure $ RSPlaced rs mdh
    RewardsWithdrawn key -> do
      key' <- pubKeyHashFromPlutus' key
      pure $ RSWithdrawn key' $ fromJust mdh

{-# INLINEABLE insertRewardsInOptionalCache #-}
insertRewardsInOptionalCache :: HotKeyQueryMonad m => GYDatumHash -> Rewards -> m ()
insertRewardsInOptionalCache dhStart rs = do
  mcache <- ask
  case mcache of
    Nothing -> pure ()
    Just cache -> do
      mrs <- liftIO $ cGet cache dhStart
      case mrs of
        Just (RSAcc _) -> pure ()
        _ -> void $ cPut' cache dhStart (RSAcc rs)

calculateRewards
  :: forall m
   . HotKeyQueryMonad m
  => Reconstructor m
  -> GYDatumHash
  -> m Rewards
calculateRewards reconstruct dhStart = do
  rs <- go dhStart
  insertRewardsInOptionalCache dhStart rs
  pure rs
  where
    go :: GYDatumHash -> m Rewards
    go dh = do
      step <- reconstruct dh
      case step of
        RSPlaced rs Nothing -> pure rs
        RSPlaced rs (Just dh') -> do
          rs' <- go dh'
          pure $! rs <> rs'
        RSWithdrawn key dh' -> do
          rs <- go dh'
          pure $! Rewards $! Map.delete key $ getRewards rs
        RSAcc rs -> pure rs

rewardsInfoToDatum :: RewardsInfo -> RewardsDatum
rewardsInfoToDatum RewardsInfo {..} =
  RewardsDatum
    { rdInfo = BuiltinByteString $ Text.encodeUtf8 riInfo
    , rdPrevious = datumHashToPlutus <$> riPrevious
    , rdNFT = tokenNameToPlutus riNFT
    , rdLastAction = riLastAction
    , rdHash = riHash
    }

rewardeeToKey :: Rewardee -> GYPubKeyHash
rewardeeToKey (WalletRewardee skh) = unsafeCoerce skh -- not nice!!!
rewardeeToKey (BotRewardee pkh) = pkh

rewardsForRewardee
  :: HotKeyQueryMonad m
  => GYCompiledScripts
  -> Reconstructor m
  -- ^ A reconstructor.
  -> GYPubKeyHash
  -- ^ The pubkey hash of the owner.
  -> Rewardee
  -- ^ The rewardee.
  -> GYTxOutRef
  -- ^ The reference to the rewards to check.
  -> m (Maybe GYValue)
  -- ^ The rewards for the rewardee.
rewardsForRewardee gycs reconstructor pkh rewardee ref = do
  info <- rewardsInfo gycs pkh ref
  rewardsForRewardee' reconstructor rewardee info

rewardsForRewardee'
  :: HotKeyQueryMonad m
  => Reconstructor m
  -- ^ A reconstructor.
  -> Rewardee
  -- ^ The rewardee.
  -> RewardsInfo
  -- ^ The information of rewards UTxO.
  -> m (Maybe GYValue)
  -- ^ The rewards for the rewardee.
rewardsForRewardee' reconstructor rewardee info = do
  rs <- rewardsForRewardee'' reconstructor info
  pure $ Map.lookup (rewardeeToKey rewardee) $ getRewards rs

rewardsForRewardee''
  :: HotKeyQueryMonad m
  => Reconstructor m
  -- ^ A reconstructor.
  -> RewardsInfo
  -- ^ The information of rewards UTxO.
  -> m Rewards
  -- ^ The total rewards.
rewardsForRewardee'' reconstructor info = do
  rs <- calculateRewards reconstructor $ hashDatum $ datumFromPlutusData $ rewardsInfoToDatum info
  when (hash rs /= riHash info)
    $ throwAppError
    $ GYRewardsMismatch (riHash info) rs
  pure rs

allRewardsForRewardee
  :: forall m
   . HotKeyQueryMonad m
  => GYCompiledScripts
  -> Reconstructor m
  -- ^ A reconstructor
  -> Rewardee
  -- ^ The rewardee.
  -> GYPubKeyHash
  -- ^ The pubkey hash of the rewards owner.
  -> m (Map GYTxOutRef (RewardsInfo, GYValue))
allRewardsForRewardee gycs reconstructor rewardee pkh = do
  m <- allRewardsForRewardee' gycs reconstructor rewardee pkh
  pure $ Map.map (\(ri, rs) -> (ri, getRewards rs Map.! rewardeeToKey rewardee)) m

allRewardsForRewardee'
  :: forall m
   . HotKeyQueryMonad m
  => GYCompiledScripts
  -> Reconstructor m
  -- ^ A reconstructor
  -> Rewardee
  -- ^ The rewardee.
  -> GYPubKeyHash
  -- ^ The pubkey hash of the rewards owner.
  -> m (Map GYTxOutRef (RewardsInfo, Rewards))
allRewardsForRewardee' gycs reconstructor rewardee pkh = do
  logMsg "" GYDebug $ printf "fetching all rewards for rewardee %s" $ show rewardee
  infos <- allRewardsInfos gycs pkh
  logMsg "" GYDebug $ printf "fetched all reward infos for rewardee %s" $ show rewardee
  res <- foldM f Map.empty $ Map.elems infos
  logMsg "" GYDebug $ printf "fetched all rewards for rewardee %s" $ show rewardee
  pure res
  where
    f :: Map GYTxOutRef (RewardsInfo, Rewards) -> RewardsInfo -> m (Map GYTxOutRef (RewardsInfo, Rewards))
    f m ri@RewardsInfo {..} = do
      logMsg "" GYDebug $ printf "fetching rewards for rewardee %s for ref %s" (show rewardee) riRef
      utxoRewards <- rewardsForRewardee'' reconstructor ri
      logMsg "" GYDebug $ printf "fetched rewards for rewardee %s for ref %s" (show rewardee) riRef
      pure
        $ if Map.member (rewardeeToKey rewardee) $ getRewards utxoRewards
          then Map.insert riRef (ri, utxoRewards) m
          else m

withdrawRewards
  :: HotKeyMonad m
  => GYCompiledScripts
  -> Reconstructor m
  -- ^ A reconstructor.
  -> GYPubKeyHash
  -- ^ The pubkey hash of the owner.
  -> GYTxOutRef
  -- ^ The reference to the rewards to withdraw from.
  -> Rewardee
  -- ^ The rewardee.
  -> m (GYTxSkeleton PlutusV2)
withdrawRewards gycs reconstructor pkh ref rewardee = do
  info <- rewardsInfo gycs pkh ref

  let
    datum = rewardsInfoToDatum info
    dh = hashDatum $ datumFromPlutusData datum

  rs <- calculateRewards reconstructor dh
  when (hash rs /= riHash info)
    $ throwAppError
    $ GYRewardsMismatch (riHash info) rs

  withdrawRewards' info rewardee rs

withdrawRewards'
  :: (GYTxUserQueryMonad m, HasCallStack)
  => RewardsInfo
  -- ^ The rewards UTxO to withdraw from.
  -> Rewardee
  -- ^ The rewardee.
  -> Rewards
  -- ^ Rewards represented by rewards UTxO.
  -> m (GYTxSkeleton PlutusV2)
withdrawRewards' info rewardee rs = do
  let
    key = rewardeeToKey rewardee
    key' = pubKeyHashToPlutus key
    datum = rewardsInfoToDatum info

  when (hash rs /= riHash info)
    $ throwAppError
    $ GYRewardsMismatch (riHash info) rs

  case Map.lookup key $ getRewards rs of
    Nothing -> throwAppError $ GYInvalidClaim rs rewardee
    Just value -> do
      gyLogInfo' "rewards" $ printf "\n\nwithdrawal by %s:\n%s\n" key value

      let
        newValue = riValue info `valueMinus` value
        rs' = Rewards $ Map.delete key $ getRewards rs
        newDatum =
          datum
            { rdPrevious = Just $ datumHashToPlutus $ hashDatum $ datumFromPlutusData datum
            , rdLastAction = RewardsWithdrawn key'
            , rdHash = hash rs'
            }

      pure
        $ mustBeSignedBy key
          <> mustHaveInput
            GYTxIn
              { gyTxInTxOutRef = riRef info
              , gyTxInWitness = GYTxInWitnessKey
              }
          <> mustHaveOutput
            GYTxOut
              { gyTxOutAddress = riAddress info
              , gyTxOutValue = newValue
              , gyTxOutDatum = Just (datumFromPlutusData newDatum, GYTxOutDontUseInlineDatum)
              , gyTxOutRefS = Nothing
              }
          <> mustHaveTxMetadata stampRewardsClaimed

withdrawAllRewards
  :: HotKeyMonad m
  => GYCompiledScripts
  -> Reconstructor m
  -- ^ Download Rewards from a given URL.
  -> GYPubKeyHash
  -- ^ The pubkey hash of the owner.
  -> Rewardee
  -- ^ The rewardee.
  -> m (GYTxSkeleton PlutusV2)
withdrawAllRewards gycs reconstructor pkh rewardee = do
  m <- allRewardsForRewardee' gycs reconstructor rewardee pkh

  when (Map.null m)
    $ throwAppError
    $ GYNoRewardsFor rewardee

  skeletons <- forM m $ \(ri, rs) ->
    withdrawRewards' ri rewardee rs
  pure $! foldMap' id skeletons -- Same as @fold skeletons@.

rewardeeFromAddress :: GYAddress -> Maybe Rewardee
rewardeeFromAddress addr = case (mskh, mpkh) of
  (Just skh, _) -> Just $ WalletRewardee skh
  (Nothing, Just pkh) -> Just $ BotRewardee $ toPubKeyHash pkh
  (Nothing, Nothing) -> Nothing
  where
    mskh = do
      msc <- addressToStakeCredential addr
      case msc of
        GYCredentialByKey skh -> Just skh
        GYCredentialByScript _ -> Nothing
    mpkh = do
      mpc <- addressToPaymentCredential addr
      case mpc of
        GYCredentialByKey pkh -> Just pkh
        GYCredentialByScript _ -> Nothing

toRewards :: [(GYAddress, GYValue)] -> Rewards
toRewards = Rewards . Map.mapKeysWith (<>) rewardeeToKey . Map.fromListWith (<>) . mapMaybe g
  where
    g :: (GYAddress, GYValue) -> Maybe (Rewardee, GYValue)
    g (addr, v) = do
      guard $ v /= mempty
      r <- rewardeeFromAddress addr
      pure (r, v)
