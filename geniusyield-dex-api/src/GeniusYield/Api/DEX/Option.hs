{- |
Module      : GeniusYield.Api.DEX.Option
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.com
Stability   : develop
-}
module GeniusYield.Api.DEX.Option
  ( OptionException (..)
  , OptionInfo (..)
  , optionAddress
  , optionInfos
  , optionInfo
  , createOption
  , executeOption
  , retrieveOption
  )
where

import Control.Monad.Reader.Class (MonadReader (..))
import Data.Map.Strict qualified as Map
import Data.Swagger qualified as Swagger
import Data.Text qualified as Text
import GeniusYield.HTTP.Errors
import GeniusYield.Imports
import GeniusYield.TxBuilder (GYTxMonadException (GYApplicationException))
import GeniusYield.TxBuilder.Class
import GeniusYield.Types
import Network.HTTP.Types (status400)

import GeniusYield.Api.DEX.Utils (NftInfo (..), nftInfo, nftInfo')
import GeniusYield.Api.Types
import GeniusYield.Scripts
import GeniusYield.Scripts.DEX

newtype OptionException = OptionDoesNotExist GYTxOutRef
  deriving stock Show
  deriving anyclass Exception

instance IsGYApiError OptionException where
  toApiError (OptionDoesNotExist ref) =
    GYApiError
      { gaeErrorCode = "OPTION_DOES_NOT_EXIST"
      , gaeHttpStatus = status400
      , gaeMsg = Text.pack $ printf "Option with ref %s does not exist." ref
      }

data OptionInfo = OptionInfo
  { opiRef :: !GYTxOutRef
  , opiOptionRef :: !GYTxOutRef
  , opiOptionToken :: !GYAssetClass
  , opiNFT :: !GYAssetClass
  , opiStart :: !GYTime
  , opiEnd :: !GYTime
  , opiDepositToken :: !GYAssetClass
  , opiPaymentToken :: !GYAssetClass
  , opiPrice :: !GYRational
  , opiValue :: !GYValue
  , opiDepositAmt :: !Natural
  , opiPaymentAmt :: !Natural
  , opiSellerKey :: !GYPubKeyHash
  }
  deriving stock (Generic, Show)
  deriving anyclass (Swagger.ToSchema, ToJSON)

optionValidatorM :: GYApiQueryMonad m => m (GYScript PlutusV2)
optionValidatorM = do
  GYCompiledScripts {dexNftPolicy} <- ask
  return $ optionValidator dexNftPolicy

optionAddress :: GYApiQueryMonad m => m GYAddress
optionAddress = optionValidatorM >>= scriptAddress

optionPolicyM :: GYApiQueryMonad m => m (GYScript PlutusV2)
optionPolicyM = do
  GYCompiledScripts {dexNftPolicy} <- ask
  optionPolicy dexNftPolicy <$> optionAddress

optionToken :: GYApiQueryMonad m => GYTxOutRef -> m GYAssetClass
optionToken ref = do
  pid <- mintingPolicyId <$> optionPolicyM
  return $ GYToken pid $ expectedTokenName ref

optionInfos' :: GYApiQueryMonad m => [(GYUTxO, Maybe GYDatum)] -> m (Map GYTxOutRef OptionInfo)
optionInfos' utxosWithDatums = do
  addr <- optionAddress
  flip iwither (utxosDatumsPure utxosWithDatums) $ \oref (utxoAddr, utxoVal, OptionDatum {..}) ->
    if utxoAddr /= addr
      then return Nothing
      else do
        NftInfo {..} <- nftInfo' opdRef
        let
          v = utxoVal `valueMinus` optionMinAda
          depositAmt = valueAssetClass v opdDeposit
          paymentAmt = valueAssetClass v opdPayment
        if depositAmt < 0 || paymentAmt < 0
          then return Nothing
          else do
            token' <- optionToken opdRef
            return
              $ if opdToken /= token'
                then Nothing
                else
                  Just
                    OptionInfo
                      { opiRef = oref
                      , opiOptionRef = opdRef
                      , opiOptionToken = token'
                      , opiNFT = nftToken
                      , opiStart = opdStart
                      , opiEnd = opdEnd
                      , opiDepositToken = opdDeposit
                      , opiPaymentToken = opdPayment
                      , opiPrice = opdPrice
                      , opiValue = utxoVal
                      , opiDepositAmt = fromInteger depositAmt
                      , opiPaymentAmt = fromInteger paymentAmt
                      , opiSellerKey = opdSellerKey
                      }

optionInfos :: GYApiQueryMonad m => m (Map GYTxOutRef OptionInfo)
optionInfos = do
  addr <- optionAddress
  utxosWithDatums <- utxosAtAddressesWithDatums [addr]
  optionInfos' utxosWithDatums

optionInfo :: GYApiQueryMonad m => GYTxOutRef -> m OptionInfo
optionInfo ref = do
  utxoWithDatum <- utxoAtTxOutRefWithDatum' ref
  infos <- optionInfos' [utxoWithDatum]
  case Map.lookup ref infos of
    Nothing -> throwError $ GYApplicationException $ OptionDoesNotExist ref
    Just info -> return info

optionDatumFromInfo :: OptionInfo -> OptionDatum
optionDatumFromInfo OptionInfo {..} =
  OptionDatum
    { opdRef = opiOptionRef
    , opdToken = opiOptionToken
    , opdStart = opiStart
    , opdEnd = opiEnd
    , opdDeposit = opiDepositToken
    , opdPayment = opiPaymentToken
    , opdPrice = opiPrice
    , opdSellerKey = opiSellerKey
    }

txInFromInfo :: GYApiQueryMonad m => OptionInfo -> OptionRedeemer -> m (GYTxIn PlutusV2)
txInFromInfo info@OptionInfo {..} r = do
  v <- optionValidatorM
  return
    GYTxIn
      { gyTxInTxOutRef = opiRef
      , gyTxInWitness =
          GYTxInWitnessScript
            (GYInScript v)
            (Just $ datumFromPlutusData $ optionDatumFromInfo info)
            (redeemerFromPlutusData r)
      }

createOption
  :: GYApiMonad m
  => GYTime
  -- ^ Start of the execution interval.
  -> GYTime
  -- ^ End of the execution interval.
  -> GYAssetClass
  -- ^ The deposited token.
  -> GYAssetClass
  -- ^ The payment token.
  -> GYRational
  -- ^ The price (payment tokens per deposited token).
  -> Natural
  -- ^ The deposit amount.
  -> GYPubKeyHash
  -- ^ The seller key.
  -> m (GYTxSkeleton PlutusV2)
createOption start end deposit payment price amount pkh = do
  NftInfo {..} <- nftInfo
  addr <- optionAddress
  tokenPolicy <- optionPolicyM

  let
    token = GYToken (mintingPolicyId tokenPolicy) nftName
    d =
      OptionDatum
        { opdRef = nftRef
        , opdStart = start
        , opdEnd = end
        , opdDeposit = deposit
        , opdPayment = payment
        , opdPrice = price
        , opdToken = token
        , opdSellerKey = pkh
        }
    amount' = toInteger amount
    v = optionMinAda <> valueSingleton deposit amount' <> valueSingleton nftToken 1

  return
    $ mustHaveInput
      ( GYTxIn
          { gyTxInTxOutRef = nftRef
          , gyTxInWitness = GYTxInWitnessKey
          }
      )
      <> mustHaveOutput
        ( GYTxOut
            { gyTxOutAddress = addr
            , gyTxOutValue = v
            , gyTxOutDatum = Just (datumFromPlutusData d, GYTxOutUseInlineDatum)
            , gyTxOutRefS = Nothing
            }
        )
      <> mustMint (GYMintScript nftPolicy) nftRedeemer nftName 1
      <> mustMint (GYMintScript tokenPolicy) (mkOptionTokenRedeemer nftRef) nftName amount'

executeOption
  :: GYApiMonad m
  => GYTxOutRef
  -> Natural
  -> m (GYTxSkeleton PlutusV2)
executeOption ref amt = do
  let amt' = toInteger amt

  info@OptionInfo {..} <- optionInfo ref
  txIn <- txInFromInfo info $ Execute amt'
  nowSlot <- slotOfCurrentBlock
  nowTime <- slotToBeginTime nowSlot
  laterSlot <- enclosingSlotFromTime' $ addSeconds nowTime 120 -- two minutes from now
  endSlot <- enclosingSlotFromTime' $ addSeconds opiEnd (-2)
  addr <- optionAddress
  policy <- optionPolicyM

  let
    price = ceiling $ toRational amt' * toRational opiPrice
    v =
      opiValue
        <> valueNegate (valueSingleton opiDepositToken amt')
        <> valueSingleton opiPaymentToken price
    d = optionDatumFromInfo info
    tn = expectedTokenName opiOptionRef

  return
    $ mustHaveInput txIn
      <> isInvalidBefore nowSlot
      <> isInvalidAfter (min laterSlot endSlot)
      <> mustMint (GYMintScript policy) (mkOptionTokenRedeemer opiOptionRef) tn (-amt')
      <> mustHaveOutput
        GYTxOut
          { gyTxOutAddress = addr
          , gyTxOutValue = v
          , gyTxOutDatum = Just (datumFromPlutusData d, GYTxOutUseInlineDatum)
          , gyTxOutRefS = Nothing
          }

retrieveOption
  :: GYApiMonad m
  => GYTxOutRef
  -> m (GYTxSkeleton PlutusV2)
retrieveOption ref = do
  info@OptionInfo {..} <- optionInfo ref
  txIn <- txInFromInfo info Retrieve
  slot <- slotOfCurrentBlock
  NftInfo {..} <- nftInfo' opiOptionRef
  return
    $ mustBeSignedBy opiSellerKey
      <> mustHaveInput txIn
      <> isInvalidBefore slot
      <> mustMint (GYMintScript nftPolicy) (mkNftRedeemer Nothing) nftName (-1)
