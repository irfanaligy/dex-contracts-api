module GeniusYield.Api.Staking.Stake
  ( StakeInfo (..)
  , stake
  , stakeInfos
  , stakeInfosAtAddress
  , stakeInfo
  , retrieveStake
  )
where

import Control.Monad.Reader (ask)
import Data.Map.Strict qualified as Map
import Data.Swagger qualified as Swagger
import Data.Text qualified as Txt
import GeniusYield.HTTP.Errors
import GeniusYield.Imports
import GeniusYield.TxBuilder
import GeniusYield.Types
import Network.HTTP.Types.Status
import PlutusLedgerApi.V1.Address (stakingCredential)
import PlutusLedgerApi.V1.Credential (StakingCredential)

import GeniusYield.Api.Types
import GeniusYield.Scripts
import GeniusYield.Scripts.Staking

newtype StkException = StkNotAvailable GYTxOutRef
  deriving stock Show
  deriving anyclass Exception

instance IsGYApiError StkException where
  toApiError (StkNotAvailable ref) =
    GYApiError
      { gaeErrorCode = "STAKE_NOT_AVAILABLE"
      , gaeHttpStatus = status400
      , gaeMsg = Txt.pack $ "Stake not available for ref: " ++ show ref
      }

-- | Provides all relevant information for a stake UTxO.
data StakeInfo = StakeInfo
  { siRef :: !GYTxOutRef
  , siOwnerKey :: !GYPubKeyHash
  , siOwnerAddr :: !GYAddress
  , siLockedUntil :: !(Maybe GYTime)
  , siValue :: !GYValue
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Swagger.ToSchema, ToJSON)

-- | Stake the specified value until the specified time.
stake
  :: (GYApiMonad m, HasCallStack)
  => GYAddress
  -- ^ Owner address
  -> GYValue
  -- ^ The value to stake.
  -> Maybe GYTime
  -- ^ The time at which the stake can be retrieved; if 'Nothing', it can be retrieved at any time.
  -> m (GYTxSkeleton v)
stake ownAddr v mt = do
  gysc <- ask
  ownKey <- addressToPubKeyHash' ownAddr
  stakeAddr <- stakeAddress gysc
  let sd =
        StakeDatum
          { sdOwnerKey = pubKeyHashToPlutus ownKey
          , sdOwnerAddr = addressToPlutus ownAddr
          , sdLockedUntil = timeToPlutus <$> mt
          }
  return $ mustHaveOutput $ mkGYTxOut stakeAddr v (datumFromPlutusData sd)

-- | Retrieve information for all valid stakes.
stakeInfos :: (GYApiQueryMonad m, HasCallStack) => m (Map GYTxOutRef StakeInfo)
stakeInfos = do
  gysc <- ask
  stakeAddr <- stakeAddress gysc
  utxosWithDatums <- utxosAtAddressesWithDatums [stakeAddr]
  stakeInfos' utxosWithDatums

-- | Retrieve information about the stakes for the specified 'GYAddress'.
stakeInfosAtAddress :: (GYApiQueryMonad m, HasCallStack) => GYAddress -> m (Map GYTxOutRef StakeInfo)
stakeInfosAtAddress address = Map.filter f <$> stakeInfos
  where
    f :: StakeInfo -> Bool
    f stInfo = stakingCredential (addressToPlutus (siOwnerAddr stInfo)) `checkEqual` givenStakingCredential

    givenStakingCredential :: Maybe StakingCredential
    givenStakingCredential = stakingCredential $ addressToPlutus address

    checkEqual :: Maybe StakingCredential -> Maybe StakingCredential -> Bool
    checkEqual Nothing Nothing = False -- Default `Eq` instance for `Maybe` would say `Nothing == Nothing` to be `True`. Though in our case, first parameter is obtained from datum and therefore won't be `Nothing` but this is an additional check.
    checkEqual a b = a == b

{- | Retrieve information about the stake at the specified 'GYTxOutRef'.
This will throw an exception if no valid stake can be found there.
-}
stakeInfo :: (GYTxQueryMonad m, HasCallStack) => GYTxOutRef -> m StakeInfo
stakeInfo ref = do
  mutxoWithDatum <- utxoAtTxOutRefWithDatum ref
  case mutxoWithDatum of
    Nothing -> throwAppError $ StkNotAvailable ref
    Just utxoWithDatum -> do
      m <- stakeInfos' [utxoWithDatum]
      case Map.lookup ref m of
        Nothing -> throwAppError $ StkNotAvailable ref
        Just si -> return si

-- | Retrieve the stake at the specified 'GYTxOutRef'.
retrieveStake :: (GYApiMonad m, HasCallStack) => GYTxOutRef -> m (GYTxSkeleton PlutusV1)
retrieveStake ref = do
  GYCompiledScripts {stakingStakeValidator} <- ask
  StakeInfo {..} <- stakeInfo ref
  let sd =
        StakeDatum
          { sdOwnerKey = pubKeyHashToPlutus siOwnerKey
          , sdOwnerAddr = addressToPlutus siOwnerAddr
          , sdLockedUntil = timeToPlutus <$> siLockedUntil
          }
  timing <- case siLockedUntil of
    Nothing -> return mempty
    Just _ -> isInvalidBefore <$> slotOfCurrentBlock
  return
    $ mustHaveInput
      GYTxIn
        { gyTxInTxOutRef = ref
        , gyTxInWitness =
            GYTxInWitnessScript
              (GYInScript stakingStakeValidator)
              (Just $ datumFromPlutusData sd)
              $ redeemerFromPlutusData ()
        }
      <> mustBeSignedBy siOwnerKey
      <> timing

stakeInfos' :: forall m. (GYTxQueryMonad m, HasCallStack) => [(GYUTxO, Maybe GYDatum)] -> m (Map GYTxOutRef StakeInfo)
stakeInfos' utxosWithDatums = do
  let datums = utxosDatumsPure utxosWithDatums
  iwither f datums
  where
    f :: GYTxOutRef -> (GYAddress, GYValue, StakeDatum) -> m (Maybe StakeInfo)
    f ref (_, v, StakeDatum {..}) = do
      maddr <- addressFromPlutusHushedM sdOwnerAddr
      let epkh = pubKeyHashFromPlutus sdOwnerKey
      return $ case (maddr, epkh) of
        (Just addr, Right pkh) ->
          Just
            StakeInfo
              { siRef = ref
              , siOwnerKey = pkh
              , siOwnerAddr = addr
              , siLockedUntil = timeFromPlutus <$> sdLockedUntil
              , siValue = v
              }
        _ -> Nothing
