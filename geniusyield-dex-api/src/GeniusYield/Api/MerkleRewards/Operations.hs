{-# LANGUAGE DataKinds #-}

module GeniusYield.Api.MerkleRewards.Operations
  ( rewardsPaymentCredential
  , rewardsAddress
  , allRewardsInfos
  , rewardsInfo
  , deployRewardsScript
  , placeRewards
  , clawBackRewards
  , mkReconstructor
  , calculateMerkleTree
  , rewardsForRewardee
  , allRewardsForRewardee
  , withdrawRewards
  , withdrawAllRewards
  , rewardsInfoToDatum
  , rewardeeFromAddress
  , toRewards
  , rewardsToMerkleTree
  )
where

import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GeniusYield.Imports
import GeniusYield.TxBuilder
  ( GYTxQueryMonad (networkId, utxosAtPaymentCredentialWithDatums)
  , GYTxSkeleton
  , datumHashFromPlutus'
  , gyLogInfo'
  , lookupDatum'
  , mustBeSignedBy
  , mustHaveInput
  , mustHaveOutput
  , mustHaveTxMetadata
  , mustMint
  , pubKeyHashFromPlutus'
  , throwAppError
  , tokenNameFromPlutus'
  , utxoAtTxOutRefWithDatum'
  , utxosDatumsPure
  )
import GeniusYield.Types hiding (Withdraw)
import PlutusLedgerApi.V1 (UnsafeFromData (..))
import PlutusLedgerApi.V2 (PubKeyHash)
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import Unsafe.Coerce (unsafeCoerce)

import GeniusYield.Api.DEX.Utils (NftInfo (..), nftInfo)
import GeniusYield.Api.MerkleRewards.Types
  ( GYRewardsException (..)
  , Reconstructor
  , Rewardee (..)
  , Rewards (..)
  , RewardsInfo (..)
  , RewardsStep (..)
  )
import GeniusYield.Api.OneWay (deployScript)
import GeniusYield.Api.Types (GYApiMonad, GYApiQueryMonad)
import GeniusYield.Api.Utils (stampRewardsClaimed)
import GeniusYield.Scripts.DEX (mkNftRedeemer)
import GeniusYield.Scripts.MerkleRewards
  ( LastRewardsAction (..)
  , RewardsAction (ClawBack, Withdraw)
  , RewardsDatum (..)
  , rewardsPolicy
  , rewardsValidator
  )
import GeniusYield.Scripts.MerkleRewards.Hashable (Hashable (..))
import GeniusYield.Scripts.MerkleRewards.MerkleTree (MerkleTree, buildMerkleTree, depth, merkleProof)

rewardsPaymentCredential :: GYApiQueryMonad m => GYPubKeyHash -> m GYPaymentCredential
rewardsPaymentCredential pkh = do
  validator <- rewardsValidator pkh
  pure $ GYCredentialByScript $ scriptHash $ validatorToScript validator

rewardsAddress
  :: (GYApiQueryMonad m, HasCallStack)
  => GYPubKeyHash
  -- ^ The pubkey hash of the owner.
  -> Maybe GYStakeKeyHash
  -- ^ Optional stake key hash to use.
  -> m GYAddress
rewardsAddress pkh mskh = do
  nid <- networkId
  pc <- rewardsPaymentCredential pkh
  pure
    $ addressFromCredential
      nid
      pc
      (GYCredentialByKey <$> mskh)

rewardsInfos
  :: forall m
   . (GYApiQueryMonad m, HasCallStack)
  => [(GYUTxO, Maybe GYDatum)]
  -> m (Map GYTxOutRef RewardsInfo)
rewardsInfos utxosWithDatums = iwither f $ utxosDatumsPure utxosWithDatums
  where
    f :: GYTxOutRef -> (GYAddress, GYValue, RewardsDatum) -> m (Maybe RewardsInfo)
    f ref (addr, value, RewardsDatum {..}) = do
      datumHash <- traverse datumHashFromPlutus' rdPrevious
      nft <- tokenNameFromPlutus' rdNFT
      pid <- mintingPolicyId <$> rewardsPolicy

      pure $ do
        guard
          $ rdDepth >= 0
            && valueAssetClass value (GYToken pid nft) == 1

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
                , riRoot = rdRoot
                , riDepth = fromInteger rdDepth
                , riLastAction = rdLastAction
                }

allRewardsInfos :: (GYApiQueryMonad m, HasCallStack) => GYPubKeyHash -> m (Map GYTxOutRef RewardsInfo)
allRewardsInfos pkh = do
  pc <- rewardsPaymentCredential pkh
  utxosWithDatums <- utxosAtPaymentCredentialWithDatums pc Nothing
  rewardsInfos utxosWithDatums

rewardsInfo :: (GYApiQueryMonad m, HasCallStack) => GYTxOutRef -> m RewardsInfo
rewardsInfo ref = do
  utxosWithDatums <- pure <$> utxoAtTxOutRefWithDatum' ref
  m <- rewardsInfos utxosWithDatums
  pure $ m Map.! ref

deployRewardsScript
  :: (GYApiMonad m, HasCallStack)
  => GYPubKeyHash
  -- ^ The pubkey hash of the owner.
  -> m (GYTxSkeleton PlutusV2)
deployRewardsScript pkh =
  deployScript . validatorToScript
    <$> rewardsValidator pkh

placeRewards
  :: (GYApiMonad m, HasCallStack)
  => GYPubKeyHash
  -- ^ The pubkey hash of the owner.
  -> Maybe GYStakeKeyHash
  -- ^ Optional stake key hash to use.
  -> Text
  -- ^ Information describing these rewards.
  -> String
  -- ^ The URL containing the rewards information.
  -> MerkleTree (PubKeyHash, GYValue)
  -- ^ The Merkle Tree with the rewards information.
  -> m (GYTxSkeleton PlutusV2)
placeRewards pkh mskh info url tree = do
  NftInfo {..} <- nftInfo
  addr <- rewardsAddress pkh mskh

  let
    nft = valueSingleton nftToken 1
    value = foldMap snd tree <> nft
    datum =
      datumFromPlutusData
        RewardsDatum
          { rdInfo = BuiltinByteString $ Text.encodeUtf8 info
          , rdPrevious = Nothing
          , rdNFT = tokenNameToPlutus nftName
          , rdRoot = hash tree
          , rdDepth = toInteger $ depth tree
          , rdLastAction = Placed $ BuiltinByteString $ Text.encodeUtf8 $ Text.pack url
          }

  pure
    $ mustMint (GYMintScript nftPolicy) nftRedeemer nftName 1
      <> mustHaveInput
        GYTxIn
          { gyTxInTxOutRef = nftRef
          , gyTxInWitness = GYTxInWitnessKey
          }
      <> mustBeSignedBy pkh
      <> mustHaveOutput
        GYTxOut
          { gyTxOutAddress = addr
          , gyTxOutValue = value
          , gyTxOutDatum = Just (datum, GYTxOutDontUseInlineDatum)
          , gyTxOutRefS = Nothing
          }

clawBackRewards
  :: (GYApiMonad m, HasCallStack)
  => GYPubKeyHash
  -- ^ The pubkey hash of the owner.
  -> Maybe GYTxOutRef
  -- ^ Optional reference to the Rewards script.
  -> GYTxOutRef
  -- ^ The reference to the rewards to claw back.
  -> m (GYTxSkeleton PlutusV2)
clawBackRewards pkh msref ref = do
  info <- rewardsInfo ref
  policy <- rewardsPolicy
  inScript <- rewardsInScript pkh msref

  pure
    $ mustBeSignedBy pkh
      <> mustMint (GYMintScript policy) (mkNftRedeemer Nothing) (riNFT info) (-1)
      <> mustHaveInput
        GYTxIn
          { gyTxInTxOutRef = ref
          , gyTxInWitness =
              GYTxInWitnessScript
                inScript
                (Just $ datumFromPlutusData $ rewardsInfoToDatum info)
                (redeemerFromPlutusData ClawBack)
          }

mkReconstructor
  :: (GYTxQueryMonad m, HasCallStack)
  => (String -> m Rewards)
  -- ^ Download Rewards from a given URL.
  -> Reconstructor m
mkReconstructor downloadRewards dh = do
  d <- lookupDatum' dh
  let RewardsDatum {..} = unsafeFromBuiltinData $ datumToPlutus' d
  case rdLastAction of
    Placed (BuiltinByteString bs) -> do
      let url = Text.unpack $ decodeUtf8Lenient bs
      rs <- downloadRewards url
      pure $ RSPlaced rs
    Withdrew key -> do
      key' <- pubKeyHashFromPlutus' key
      dh' <- datumHashFromPlutus' $ fromJust rdPrevious
      pure $ RSWithdrawn key' dh'

calculateMerkleTree
  :: (GYTxQueryMonad m, HasCallStack)
  => Reconstructor m
  -> GYDatumHash
  -> m (MerkleTree (PubKeyHash, GYValue))
calculateMerkleTree reconstruct dh = do
  step <- reconstruct dh
  case step of
    RSPlaced rs -> pure $ rewardsToMerkleTree rs
    RSWithdrawn key dh' -> do
      tree' <- calculateMerkleTree reconstruct dh'
      let
        key' = pubKeyHashToPlutus key
        (_, tree, _) = fromJust $ merkleProof ((== key') . fst) tree'
      pure tree

rewardsForRewardee
  :: (GYApiQueryMonad m, HasCallStack)
  => Reconstructor m
  -- ^ A reconstructor.
  -> Rewardee
  -- ^ The rewardee.
  -> GYTxOutRef
  -- ^ The reference to the rewards to check.
  -> m (Maybe GYValue)
  -- ^ The rewards for the rewardee.
rewardsForRewardee reconstructor rewardee ref = do
  info <- rewardsInfo ref
  tree <- calculateMerkleTree reconstructor $ hashDatum $ datumFromPlutusData $ rewardsInfoToDatum info
  let key = pubKeyHashToPlutus $ rewardeeToKey rewardee
  pure $ do
    ((_, value), _, _) <- merkleProof ((== key) . fst) tree
    pure value

allRewardsForRewardee
  :: forall m
   . (GYApiQueryMonad m, HasCallStack)
  => Reconstructor m
  -- ^ A reconstructor
  -> Rewardee
  -- ^ The rewardee.
  -> GYPubKeyHash
  -- ^ The pubkey hash of the rewards owner.
  -> m (Map GYTxOutRef (Text, GYValue))
allRewardsForRewardee reconstructor rewardee pkh = do
  infos <- allRewardsInfos pkh
  foldM f Map.empty $ Map.elems infos
  where
    f :: Map GYTxOutRef (Text, GYValue) -> RewardsInfo -> m (Map GYTxOutRef (Text, GYValue))
    f m RewardsInfo {..} = do
      mvalue <- rewardsForRewardee reconstructor rewardee riRef
      pure $ case mvalue of
        Nothing -> m
        Just value -> Map.insert riRef (riInfo, value) m

withdrawRewards
  :: (GYApiMonad m, HasCallStack)
  => Reconstructor m
  -- ^ A reconstructor.
  -> GYPubKeyHash
  -- ^ The pubkey hash of the owner.
  -> Maybe GYTxOutRef
  -- ^ Optional reference to the Rewards script.
  -> GYTxOutRef
  -- ^ The reference to the rewards to withdraw from.
  -> Rewardee
  -- ^ The rewardee.
  -> GYAddress
  -- ^ The address to withdraw to.
  -> m (GYTxSkeleton PlutusV2)
withdrawRewards reconstructor pkh msref ref rewardee addr = do
  info <- rewardsInfo ref
  inScript <- rewardsInScript pkh msref

  let
    key = rewardeeToKey rewardee
    key' = pubKeyHashToPlutus key
    datum = rewardsInfoToDatum info
    dh = hashDatum $ datumFromPlutusData datum

  tree <- calculateMerkleTree reconstructor dh

  case merkleProof ((== key') . fst) tree of
    Nothing -> throwAppError $ GYInvalidClaim tree rewardee
    Just ((_, value), tree', proof) -> do
      gyLogInfo' "rewards" $ printf "\n\nwithdrawal by %s:\n%s\n" key value

      let
        redeemer =
          redeemerFromPlutusData
            $ Withdraw key' (datumHashToPlutus $ hashDatum $ datumFromPlutusData $ valueToPlutus value) proof
        newValue = riValue info `valueMinus` value
        newDatum =
          datum
            { rdPrevious = Just $ datumHashToPlutus $ hashDatum $ datumFromPlutusData datum
            , rdRoot = hash tree'
            , rdLastAction = Withdrew key'
            }

      pure
        $ mustBeSignedBy key
          <> mustHaveInput
            GYTxIn
              { gyTxInTxOutRef = ref
              , gyTxInWitness =
                  GYTxInWitnessScript
                    inScript
                    (Just $ datumFromPlutusData datum)
                    redeemer
              }
          <> mustHaveOutput
            GYTxOut
              { gyTxOutAddress = riAddress info
              , gyTxOutValue = newValue
              , gyTxOutDatum = Just (datumFromPlutusData newDatum, GYTxOutDontUseInlineDatum)
              , gyTxOutRefS = Nothing
              }
          <> mustHaveOutput
            GYTxOut
              { gyTxOutAddress = addr
              , gyTxOutValue = value
              , gyTxOutDatum = Just (datumFromPlutusData datum, GYTxOutDontUseInlineDatum)
              , gyTxOutRefS = Nothing
              }
          <> mustHaveOutput
            GYTxOut
              { gyTxOutAddress = addr
              , gyTxOutValue = mempty
              , gyTxOutDatum = Just (datumFromPlutusData $ valueToPlutus value, GYTxOutDontUseInlineDatum)
              , gyTxOutRefS = Nothing
              }
          <> mustHaveTxMetadata stampRewardsClaimed

withdrawAllRewards
  :: (GYApiMonad m, HasCallStack)
  => Reconstructor m
  -- ^ Download Rewards from a given URL.
  -> GYPubKeyHash
  -- ^ The pubkey hash of the owner.
  -> Maybe GYTxOutRef
  -- ^ Optional reference to the Rewards script.
  -> Rewardee
  -- ^ The rewardee.
  -> GYAddress
  -- ^ The address to withdraw to.
  -> m (GYTxSkeleton PlutusV2)
withdrawAllRewards reconstructor pkh msref rewardee addr = do
  m <- allRewardsForRewardee reconstructor rewardee pkh
  skeletons <- forM (Map.keys m) $ \ref ->
    withdrawRewards reconstructor pkh msref ref rewardee addr
  pure $ mconcat skeletons

rewardsInScript :: GYApiQueryMonad m => GYPubKeyHash -> Maybe GYTxOutRef -> m (GYInScript PlutusV2)
rewardsInScript pkh msref = do
  validator <- rewardsValidator pkh
  pure $ case msref of
    Nothing -> GYInScript validator
    Just sref -> GYInReference sref $ validatorToScript validator

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

rewardeeToKey :: Rewardee -> GYPubKeyHash
rewardeeToKey (WalletRewardee skh) = unsafeCoerce skh -- not nice!!!
rewardeeToKey (BotRewardee pkh) = pkh

toRewards :: [(GYAddress, GYValue)] -> Rewards
toRewards = Rewards . Map.mapKeysWith (<>) rewardeeToKey . Map.fromListWith (<>) . mapMaybe g
  where
    g :: (GYAddress, GYValue) -> Maybe (Rewardee, GYValue)
    g (addr, v) = do
      guard $ v /= mempty
      r <- rewardeeFromAddress addr
      pure (r, v)

rewardsToMerkleTree :: Rewards -> MerkleTree (PubKeyHash, GYValue)
rewardsToMerkleTree = fst . buildMerkleTree . map (first pubKeyHashToPlutus) . Map.toList . getRewards

rewardsInfoToDatum :: RewardsInfo -> RewardsDatum
rewardsInfoToDatum RewardsInfo {..} =
  RewardsDatum
    { rdInfo = BuiltinByteString $ Text.encodeUtf8 riInfo
    , rdPrevious = datumHashToPlutus <$> riPrevious
    , rdNFT = tokenNameToPlutus riNFT
    , rdRoot = riRoot
    , rdDepth = toInteger riDepth
    , rdLastAction = riLastAction
    }
