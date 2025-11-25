module GeniusYield.Api.Vesting.Operations
  ( vestingAddress
  , mainnetRef
  , preProdRef
  , vestingInfos
  , allVestingInfos
  , unlockableVestingInfos
  , placeVestings
  , deployVestingValidator
  , cancelVestings
  , unlockVestings
  )
where

import Data.Map.Strict qualified as Map
import GeniusYield.Imports
import GeniusYield.TxBuilder
import GeniusYield.Types

import GeniusYield.Api.OneWay
import GeniusYield.Api.Vesting.Types
import GeniusYield.Scripts.Vesting

vestingAddress :: (GYTxQueryMonad m, HasCallStack) => m GYAddress
vestingAddress = scriptAddress vestingValidator

mainnetRef, preProdRef :: GYTxOutRef
mainnetRef = "07c0382b3e6937e9d527d6b8f1fc174d377362224892cd5d90c8e7ebdc69b26b#0"
preProdRef = "e323e213a5ea782c5b7115d6e368fdb8f3259a9c4014378d219d37b5bc9f17e7#0"

vestingInfos :: forall m. (GYTxQueryMonad m, HasCallStack) => [(GYUTxO, Maybe GYDatum)] -> m (Map GYTxOutRef VestingInfo)
vestingInfos utxosWithDatums = iwither f $ utxosDatumsPure utxosWithDatums
  where
    f :: GYTxOutRef -> (GYAddress, GYValue, VestingDatum) -> m (Maybe VestingInfo)
    f utxoRef (_, v, d) = flip catchError (const $ return Nothing) $ do
      recipient <- addressFromPlutus' $ vdRecipient d
      deposit <- addressFromPlutus' $ vdDeposit d
      pkh <- pubKeyHashFromPlutus' $ vdCancelKey d
      return
        $ Just
          VestingInfo
            { viRef = utxoRef
            , viRecipient = recipient
            , viDeposit = deposit
            , viCancelKey = pkh
            , viCancelTime = timeFromPlutus $ vdCancelTime d
            , viUnlockTime = timeFromPlutus $ vdUnlockTime d
            , viValue = v `valueMinus` minDeposit
            }

allVestingInfos :: forall m. (GYTxQueryMonad m, HasCallStack) => m (Map GYTxOutRef VestingInfo)
allVestingInfos = do
  addr <- vestingAddress
  gyLogDebug' "" $ printf "looking for vestings at address %s" addr
  utxosWithDatums <- utxosAtAddressesWithDatums [addr]
  infos <- vestingInfos utxosWithDatums
  gyLogDebug' "" $ printf "found %d vesting(s)" $ Map.size infos
  return infos

unlockableVestingInfos :: forall m. (GYTxQueryMonad m, HasCallStack) => m (Map GYTxOutRef VestingInfo)
unlockableVestingInfos = do
  infos <- allVestingInfos
  slotNow <- slotOfCurrentBlock
  timeNow <- slotToEndTime slotNow
  gyLogDebug' "" $ printf "current slot: %s, corresponding time: %s" slotNow timeNow
  let unlockable = Map.filter (\VestingInfo {..} -> viUnlockTime < timeNow) infos
  gyLogDebug' "" $ printf "found %d unlockable vesting(s)" $ Map.size unlockable
  return unlockable

placeVestings
  :: (GYTxUserQueryMonad m, HasCallStack)
  => GYAddress
  -- ^ The deposit address.
  -> GYPubKeyHash
  -- ^ The cancel key.
  -> [Recipient]
  -- ^ The vestings to place.
  -> m (GYTxSkeleton PlutusV2)
placeVestings deposit pkh vs = do
  addr <- vestingAddress
  end <- twoMinutesFromNow
  return $ foldMap (f addr) vs <> isInvalidAfter end -- the transaction should only be valid for two minutes, so that if after two minutes, it is not on the blockchain, it never will be
  where
    f :: GYAddress -> Recipient -> GYTxSkeleton PlutusV2
    f addr Recipient {..} =
      mustHaveOutput
        $ GYTxOut
          { gyTxOutAddress = addr
          , gyTxOutValue = valueSingleton recToken (fromIntegral recAmount) <> minDeposit
          , gyTxOutDatum =
              Just
                ( datumFromPlutusData
                    $ VestingDatum
                      { vdRecipient = addressToPlutus recAddress
                      , vdDeposit = addressToPlutus deposit
                      , vdCancelKey = pubKeyHashToPlutus pkh
                      , vdCancelTime = timeToPlutus recCancelTime
                      , vdUnlockTime = timeToPlutus recUnlockTime
                      }
                , GYTxOutUseInlineDatum
                )
          , gyTxOutRefS = Nothing
          }

deployVestingValidator :: GYTxSkeleton PlutusV2
deployVestingValidator = deployScript $ validatorToScript vestingValidator

cancelVestings
  :: (GYTxUserQueryMonad m, HasCallStack)
  => Maybe GYTxOutRef
  -- ^ The UTxO for Reference Script.
  -> [VestingInfo]
  -- ^ The vestings to cancel.
  -> m (GYTxSkeleton PlutusV2)
cancelVestings mScriptRef vs = do
  nid <- networkId
  twoMinutesFromNow' <- twoMinutesFromNow
  minEndTime <-
    foldl'
      ( \currentMin VestingInfo {..} -> do
          currentMin' <- currentMin
          viCancelTime' <- enclosingSlotFromTime' viCancelTime
          return $ min currentMin' viCancelTime'
      )
      (pure twoMinutesFromNow')
      vs
  return $ isInvalidAfter minEndTime <> foldMap (input mScriptRef nid Cancel) vs <> foldMap sig vs
  where
    sig :: VestingInfo -> GYTxSkeleton PlutusV2
    sig VestingInfo {..} = mustBeSignedBy viCancelKey

unlockVestings
  :: (GYTxUserQueryMonad m, HasCallStack)
  => Maybe GYTxOutRef
  -- ^ The UTxO for Reference Script.
  -> [VestingInfo]
  -- ^ The vestings to unlock.
  -> m (GYTxSkeleton PlutusV2)
unlockVestings mScriptRef vs = do
  nid <- networkId
  now <- slotOfCurrentBlock
  return $ isInvalidBefore now <> foldMap (input mScriptRef nid Unlock) vs <> foldMap outputs vs
  where
    outputs :: VestingInfo -> GYTxSkeleton PlutusV2
    outputs VestingInfo {..} =
      mustHaveOutput
        GYTxOut
          { gyTxOutAddress = viRecipient
          , gyTxOutValue = viValue
          , gyTxOutDatum = Just (datumFromPlutusData $ txOutRefToPlutus viRef, GYTxOutUseInlineDatum)
          , gyTxOutRefS = Nothing
          }
        <> mustHaveOutput
          GYTxOut
            { gyTxOutAddress = viDeposit
            , gyTxOutValue = minDeposit
            , gyTxOutDatum = Just (datumFromPlutusData $ txOutRefToPlutus viRef, GYTxOutUseInlineDatum)
            , gyTxOutRefS = Nothing
            }

twoMinutesFromNow :: (GYTxQueryMonad m, HasCallStack) => m GYSlot
twoMinutesFromNow = do
  time <- flip addSeconds 120 <$> (slotOfCurrentBlock >>= slotToBeginTime)
  mend <- enclosingSlotFromTime time
  case mend of
    Nothing -> throwError $ GYApplicationException $ GYTimeBeforeSystemStartException time
    Just end -> return end

input :: Maybe GYTxOutRef -> GYNetworkId -> VestingAction -> VestingInfo -> GYTxSkeleton PlutusV2
input mRef nid va vi@VestingInfo {..} =
  -- GYInReference (s nid) $ validatorToScript vestingValidator
  mustHaveInput
    GYTxIn
      { gyTxInTxOutRef = viRef
      , gyTxInWitness =
          GYTxInWitnessScript
            (GYInReference ref' $ validatorToScript vestingValidator)
            (Just $ datumFromPlutusData $ vestingDatumFromInfo vi)
            (redeemerFromPlutusData va)
      }
  where
    ref' :: GYTxOutRef
    ref' = case nid of
      GYMainnet -> fromMaybe mainnetRef mRef
      GYTestnetPreprod -> fromMaybe preProdRef mRef
      _anyOtherNetwork -> fromMaybe (error "No Reference Script provided.") mRef

vestingDatumFromInfo :: VestingInfo -> VestingDatum
vestingDatumFromInfo VestingInfo {..} =
  VestingDatum
    { vdRecipient = addressToPlutus viRecipient
    , vdDeposit = addressToPlutus viDeposit
    , vdCancelKey = pubKeyHashToPlutus viCancelKey
    , vdCancelTime = timeToPlutus viCancelTime
    , vdUnlockTime = timeToPlutus viUnlockTime
    }
