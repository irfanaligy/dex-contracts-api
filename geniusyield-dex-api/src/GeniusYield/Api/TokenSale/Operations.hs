{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GeniusYield.Api.TokenSale.Operations
  ( GetOrderInfoException (..)
  , PlaceOrderException (..)
  , FillOrderException (..)
  , orderAddress
  , orders
  , placeOrder
  , cancelOrder
  , fillOrders
  , priceAndFees

    -- * Conversion
  , tspToScripts
  , validateSaleRoundInfo
  )
where

import Control.Monad.Reader (ask)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Txt
import GeniusYield.HTTP.Errors
import GeniusYield.Imports
import GeniusYield.TxBuilder
import GeniusYield.Types
import Network.HTTP.Types.Status
import PlutusTx.Ratio (fromGHC)

import GeniusYield.Api.TokenSale.Types
import GeniusYield.Api.Types (GYApiMonad, GYApiQueryMonad)
import GeniusYield.Scripts
import GeniusYield.Scripts qualified as Scripts
import GeniusYield.Scripts.TokenSale qualified as Scripts

-- | Exceptions raised in the 'getOrderInfo' endpoint.
data GetOrderInfoException
  = -- | No utxo found for given ref.
    NoOrderForRef GYTxOutRef
  | -- | Not a order UTxO (incorrect/missing datum).
    InvalidOrderUtxo GYUTxO
  deriving stock Show
  deriving anyclass Exception

instance IsGYApiError GetOrderInfoException where
  toApiError (NoOrderForRef ref) =
    GYApiError
      { gaeErrorCode = "ORDER_NOT_FOUND"
      , gaeHttpStatus = status404
      , gaeMsg = Txt.pack $ "No order found for ref: " ++ show ref
      }
  toApiError (InvalidOrderUtxo utxo) =
    GYApiError
      { gaeErrorCode = "INVALID_ORDER"
      , gaeHttpStatus = status400
      , gaeMsg = Txt.pack $ "Not a valid order: " ++ show utxo
      }

-- | Exceptions raised in the 'placeOrder' endpoint.
data PlaceOrderException
  = -- | Specified order amount is lower than min allocation.
    AmountTooSmall {poeAmount :: !Natural, poeMinAllocation :: !Natural}
  | -- | Order is being placed before sale has started.
    TooEarlyPlace {poeBeginSale :: !GYTime, poeNow :: !GYTime}
  | -- | Order placing has a validity range upper bound after sale has ended.
    TooLatePlace {poeEndSale :: !GYTime, poeValidLatest :: !GYTime}
  deriving stock Show
  deriving anyclass Exception

instance IsGYApiError PlaceOrderException where
  toApiError (AmountTooSmall amt minAlloc) =
    GYApiError
      { gaeErrorCode = "ORDER_AMOUNT_TOO_SMALL"
      , gaeHttpStatus = status400
      , gaeMsg = Txt.pack $ "Order amount must be at least: " ++ show minAlloc ++ "\nBut it was: " ++ show amt
      }
  toApiError (TooEarlyPlace beginSaleT nowT) =
    GYApiError
      { gaeErrorCode = "TOO_EARLY_PLACE"
      , gaeHttpStatus = status400
      , gaeMsg =
          Txt.pack
            $ "Offer cannot be placed before: "
              ++ show beginSaleT
              ++ "\ntime now: "
              ++ show nowT
      }
  toApiError (TooLatePlace endSaleT afterT) =
    GYApiError
      { gaeErrorCode = "TOO_LATE_PLACE"
      , gaeHttpStatus = status400
      , gaeMsg =
          Txt.pack
            $ "Offer cannot be placed after: "
              ++ show endSaleT
              ++ "\nvalidity time upper bound: "
              ++ show afterT
      }

-- | Exceptions raised in the 'fillOrder' endpoint.
data FillOrderException
  = {- | Offer identified by 'foeOrderRef',
        which is offering 'foeOfferLovelace' amount in lovelace,
        needs 'foeLovelaceDip' more amount in lovelace,
        to buy 'foeTokenCount' number of tokens
    -}
    InsufficientOffer
      { foeOrderRef :: !GYTxOutRef
      , foeOfferLovelace :: !Natural
      , foeLovelaceDip :: !Integer
      , foeTokenCount :: !Natural
      }
  | -- | Order is being filled before distribution period has started.
    TooEarlyFill {foeBeginFill :: !GYTime, foeNow :: !GYTime}
  | -- | Order filling has a validity range upper bound after distribution period has ended.
    TooLateFill {foeEndFill :: !GYTime, foeValidLatest :: !GYTime}
  deriving stock Show
  deriving anyclass Exception

instance IsGYApiError FillOrderException where
  toApiError (InsufficientOffer ref offerLovelace lovelaceDip tkCount) =
    GYApiError
      { gaeErrorCode = "INSUFFICIENT_OFFER"
      , gaeHttpStatus = status400
      , gaeMsg =
          Txt.pack
            $ "Offer given by ref: "
              ++ show ref
              ++ "\nis offering: "
              ++ show offerLovelace
              ++ " amount in lovelace"
              ++ "\nbut it needs: "
              ++ show lovelaceDip
              ++ " more lovelace"
              ++ "\nto buy: "
              ++ show tkCount
              ++ " amount in asked tokens"
      }
  toApiError (TooEarlyFill beginFillT nowT) =
    GYApiError
      { gaeErrorCode = "TOO_EARLY_FILL"
      , gaeHttpStatus = status400
      , gaeMsg =
          Txt.pack
            $ "Offer cannot be filled before: "
              ++ show beginFillT
              ++ "\ntime now: "
              ++ show nowT
      }
  toApiError (TooLateFill endFillT afterT) =
    GYApiError
      { gaeErrorCode = "TOO_LATE_FILL"
      , gaeHttpStatus = status400
      , gaeMsg =
          Txt.pack
            $ "Offer cannot be filled after: "
              ++ show endFillT
              ++ "\nvalidity time upper bound: "
              ++ show afterT
      }

tspToScripts :: TokenSaleParams -> Scripts.TokenSaleParams
tspToScripts TokenSaleParams {..} =
  Scripts.TokenSaleParams
    { Scripts.tspBeginSale = timeToPlutus tspBeginSale
    , Scripts.tspEndSale = timeToPlutus tspEndSale
    , Scripts.tspEndDistribution = timeToPlutus tspEndDistribution
    , Scripts.tspToken = assetClassToPlutus tspToken
    , Scripts.tspPrice = fromGHC (toRational tspPrice)
    , Scripts.tspSellerKey = pubKeyHashToPlutus tspSellerKey
    , Scripts.tspMinAllocation = fromIntegral tspMinAllocation
    , Scripts.tspFee = fromGHC (toRational tspFee)
    , Scripts.tspFeeAddress = addressToPlutus $ addressFromBech32 tspFeeAddress
    }

orderAddress :: (GYApiQueryMonad m, HasCallStack) => TokenSaleParams -> m GYAddress
orderAddress tsp = do
  GYCompiledScripts {tsOrderValidator = orderValidator} <- ask
  scriptAddress . orderValidator $ tspToScripts tsp

-- | Calculates the price and fees (in lovelace) for the given amount of tokens.
priceAndFees
  :: TokenSaleParams
  -- ^ The token sale parameters.
  -> Natural
  -- ^ The amount of tokens.
  -> (Natural, Natural)
  -- ^ The price and fees, both in lovelace.
priceAndFees TokenSaleParams {..} tokenCount =
  let
    priceLovelace = ceiling $ fromIntegral tokenCount * tspPrice
    feesLovelace = ceiling $ fromIntegral priceLovelace * tspFee
  in
    (priceLovelace, feesLovelace)

minLovelaceInOrder
  :: TokenSaleParams
  -> Natural
minLovelaceInOrder p@TokenSaleParams {..} =
  let (price, fees) = priceAndFees p tspMinAllocation
  in fromInteger Scripts.minTokenSaleDeposit + price + fees

orderInfos :: forall m. (GYApiQueryMonad m, HasCallStack) => TokenSaleParams -> [(GYUTxO, Maybe GYDatum)] -> m (Map GYTxOutRef OrderInfo)
orderInfos p utxosWithDatums = do
  gysc <- ask
  let xs = utxosDatumsPure utxosWithDatums
  nft <- Scripts.spt gysc $ tspToScripts p
  let nftValue = valueSingleton nft 1
  foldM (f nftValue) Map.empty $ Map.toList xs
  where
    minLovelaceInOrder' :: Integer
    minLovelaceInOrder' = fromIntegral $ minLovelaceInOrder p

    f :: GYValue -> Map GYTxOutRef OrderInfo -> (GYTxOutRef, (GYAddress, GYValue, Scripts.OrderDatum)) -> m (Map GYTxOutRef OrderInfo)
    f nftValue m (oref, (_, v, od)) = do
      let (n, v') = valueSplitAda v
      if v' == nftValue && n >= minLovelaceInOrder'
        then do
          addr' <- fmap hush $ flip addressFromPlutus (Scripts.odOwnerAddr od) <$> networkId
          let e = pubKeyHashFromPlutus $ Scripts.odOwnerKey od
          return $ case (addr', e) of
            (Just addr, Right key) ->
              let oi =
                    OrderInfo
                      { oiRef = oref
                      , oiOwnerKey = key
                      , oiOwnerAddr = addr
                      , oiLovelace = fromInteger n
                      , oiRequest = tokensForLovelace p $ fromInteger $ n - Scripts.minTokenSaleDeposit
                      }
              in Map.insert oref oi m
            _ -> m
        else return m

tokensForLovelace :: TokenSaleParams -> Natural -> Natural
tokensForLovelace p@TokenSaleParams {..} l = head $ dropWhile tooMuch [upperBound, upperBound - 1 ..]
  where
    upperBound :: Natural
    upperBound = floor $ fromIntegral l / (1 + tspFee) / tspPrice

    tooMuch :: Natural -> Bool
    tooMuch t =
      let (price, fee) = priceAndFees p t
      in price + fee > l

getOrderInfo :: (GYApiQueryMonad m, HasCallStack) => TokenSaleParams -> GYTxOutRef -> m OrderInfo
getOrderInfo p orderRef = do
  utxoWithDatum <- utxoAtTxOutRefWithDatum orderRef >>= maybe (throwAppError $ NoOrderForRef orderRef) pure
  m <- orderInfos p [utxoWithDatum]
  maybe
    (throwAppError $ InvalidOrderUtxo $ fst utxoWithDatum)
    pure
    $ Map.lookup orderRef m

orderInfoToIn :: HasCallStack => GYCompiledScripts -> TokenSaleParams -> OrderInfo -> Scripts.OrderAction -> GYTxIn PlutusV2
orderInfoToIn GYCompiledScripts {tsOrderValidator = orderValidator} p oi oa =
  GYTxIn
    { gyTxInTxOutRef = oiRef oi
    , gyTxInWitness =
        GYTxInWitnessScript
          (GYInScript . orderValidator $ tspToScripts p)
          (Just $ datumFromPlutusData od)
          $ redeemerFromPlutusData oa
    }
  where
    od :: Scripts.OrderDatum
    od =
      Scripts.OrderDatum
        { Scripts.odOwnerKey = pubKeyHashToPlutus $ oiOwnerKey oi
        , Scripts.odOwnerAddr = addressToPlutus $ oiOwnerAddr oi
        }

orderInfoBurn
  :: (GYApiQueryMonad m, HasCallStack)
  => TokenSaleParams
  -> OrderInfo
  -> m (OrderInfo, GYTxSkeleton PlutusV2)
orderInfoBurn p oi = do
  GYCompiledScripts {tsSptPolicy = sptPolicy} <- ask
  oaddr <- orderAddress p
  let
    policy = sptPolicy oaddr $ tspToScripts p
    mint = mustMint (GYMintScript policy) unitRedeemer Scripts.sptTokenName (-1)
  return (oi, mint)

orders :: forall m. (GYApiQueryMonad m, HasCallStack) => TokenSaleParams -> m (Map GYTxOutRef OrderInfo)
orders p = do
  oaddr <- orderAddress p
  utxosWithDatums <- utxosAtAddressesWithDatums [oaddr]
  orderInfos p utxosWithDatums

placeOrder
  :: (GYApiMonad m, HasCallStack)
  => GYAddress
  -- ^ Owner of the order
  -> TokenSaleParams
  -- ^ The token sale parameters.
  -> Natural
  -- ^ The amount of tokens to buy.
  -> m (GYTxSkeleton PlutusV2)
  -- ^ Returns the skeleton of a transaction that will place the order.
placeOrder addr p@TokenSaleParams {..} n
  | n < tspMinAllocation = throwAppError $ AmountTooSmall n tspMinAllocation
  | otherwise = do
      gysc@GYCompiledScripts {tsSptPolicy = sptPolicy} <- ask
      oaddr <- orderAddress p
      pkh <- addressToPubKeyHash' addr
      let
        p' = tspToScripts p
        policy = sptPolicy oaddr p'
      nft <- Scripts.spt gysc p'
      outAddr <- orderAddress p
      now <- slotOfCurrentBlock
      let od =
            Scripts.OrderDatum
              { odOwnerKey = pubKeyHashToPlutus pkh
              , odOwnerAddr = addressToPlutus addr
              }

      let
        (price, fees) = priceAndFees p n
        v =
          valueFromLovelace (Scripts.minTokenSaleDeposit + fromIntegral (price + fees))
            <> valueSingleton nft 1

      -- Give the transaction five minutes to be signed, submitted and validated.
      after <- advanceSlot' now 300

      checkSlotRange
        tspBeginSale
        tspEndSale
        now
        after
        (throwAppError . TooEarlyPlace tspBeginSale)
        (throwAppError . TooLatePlace tspEndSale)

      gyLogDebug' "TokenSale.placeOrder" $ printf "now(slot) = %s, after(slot) = %s" (show now) (show after)

      return
        $ mustHaveOutput (mkGYTxOut outAddr v (datumFromPlutusData od))
          <> mustMint (GYMintScript policy) unitRedeemer Scripts.sptTokenName 1
          <> isInvalidBefore now
          <> isInvalidAfter after

cancelOrder :: (GYApiMonad m, HasCallStack) => TokenSaleParams -> GYTxOutRef -> m (GYTxSkeleton PlutusV2)
cancelOrder p orderRef = do
  gysc <- ask
  orderInfo <- getOrderInfo p orderRef
  (oi, mint) <- orderInfoBurn p orderInfo
  now <- slotOfCurrentBlock

  return
    $ mustHaveInput (orderInfoToIn gysc p oi Scripts.Cancel)
      <> mint
      <> mustBeSignedBy (oiOwnerKey oi)
      <> isInvalidBefore now

fillOrders
  :: forall m
   . (GYApiMonad m, HasCallStack)
  => TokenSaleParams
  -- ^ The token sale parameters.
  -> Map GYTxOutRef (Maybe OrderInfo, Natural)
  -- ^ Which orders to fill and with how many tokens.
  -> Maybe GYAddress
  -- ^ Address to receive the payment; if 'Nothing', payment will go to the distributor.
  -> m (GYTxSkeleton PlutusV2)
fillOrders p@TokenSaleParams {..} tokenCounts mPaymentAddr = do
  now <- slotOfCurrentBlock
  after <- advanceSlot' now 300

  checkSlotRange
    tspEndSale
    tspEndDistribution
    now
    after
    (throwAppError . TooEarlyFill tspEndSale)
    (throwAppError . TooLateFill tspEndDistribution)

  (xs, x) <- foldM g (mempty, 0) $ Map.toList tokenCounts
  let s =
        mustBeSignedBy tspSellerKey
          <> isInvalidBefore now
          <> isInvalidAfter after
          <> xs
  return $ case mPaymentAddr of
    Just paymentAddr
      | x > 0 -> s <> mustHaveOutput (mkGYTxOutNoDatum paymentAddr (valueFromLovelace $ fromIntegral x))
    _ -> s
  where
    g :: (GYTxSkeleton PlutusV2, Natural) -> (GYTxOutRef, (Maybe OrderInfo, Natural)) -> m (GYTxSkeleton PlutusV2, Natural)
    g (s, x) (orderRef, (o, tokenCount)) = do
      gysc <- ask
      orInfo <- exactOrder orderRef o
      (oi@OrderInfo {..}, mint) <- orderInfoBurn p orInfo

      let
        (price, fees) = priceAndFees p tokenCount
        change = fromIntegral oiLovelace - fromIntegral (price + fees)
        v1 =
          valueSingleton tspToken (fromIntegral tokenCount)
            <> valueFromLovelace change
        v2 = valueFromLovelace $ fromIntegral fees
        d = datumFromPlutusData $ txOutRefToPlutus orderRef

      if change < 0
        then
          throwAppError
            $ InsufficientOffer
              orderRef
              oiLovelace
              (-change)
              tokenCount
        else
          return
            ( s
                <> mustHaveInput (orderInfoToIn gysc p oi Scripts.Fill)
                <> mustHaveOutput (mkGYTxOut oiOwnerAddr v1 d)
                <> mustHaveOutput (mkGYTxOut (addressFromBech32 tspFeeAddress) v2 d)
                <> mint
            , x + price
            )

    exactOrder :: GYTxOutRef -> Maybe OrderInfo -> m OrderInfo
    exactOrder _ (Just oi) = pure oi
    exactOrder ref _ = getOrderInfo p ref

-- Utility function for slot range errors
checkSlotRange
  :: GYTxQueryMonad m
  => GYTime
  -> GYTime
  -> GYSlot
  -> GYSlot
  -> (GYTime -> m ())
  -> (GYTime -> m ())
  -> m ()
checkSlotRange start end now after tooEarlyAct tooLateAct = do
  nowTime <- slotToBeginTime now
  afterTime <- slotToEndTime after

  unless (start <= nowTime)
    $ tooEarlyAct nowTime
  unless (afterTime <= end)
    $ tooLateAct afterTime

-- | Tokensale params validation
data RoundException
  = --  | BeginSale is same as or is later than EndSale
    ToEarlyEndSale {reBeginSale :: GYTime, reEndSale :: GYTime}
  | --  | Token distribution is same as or is before EndSale
    ToEarlyDistribution {reEndSale :: GYTime, reEndDistribution :: GYTime}
  | NonPositiveMinAllocation Natural
  | InvalidFee GYRational
  deriving stock Show
  deriving anyclass Exception

instance IsGYApiError RoundException where
  toApiError (ToEarlyEndSale beginSale endSale) =
    GYApiError
      { gaeErrorCode = "TOO_EARLY_END_SALE"
      , gaeHttpStatus = status400
      , gaeMsg = Txt.pack $ "Token Sale must end after token sale is started. but " <> "Sale begins: " <> show beginSale <> " and  ends " <> show endSale
      }
  toApiError (ToEarlyDistribution endSale endDistribution) =
    GYApiError
      { gaeErrorCode = "TOO_EARLY_DISTRIBUTION"
      , gaeHttpStatus = status400
      , gaeMsg = Txt.pack $ "Distribution must end after token sale. Sale Ends :" <> show endSale <> " and Distribution ends: " <> show endDistribution
      }
  toApiError (NonPositiveMinAllocation minAlloc) =
    GYApiError
      { gaeErrorCode = "NON_POSITIVE_MIN_ALLOCATION"
      , gaeHttpStatus = status400
      , gaeMsg = Txt.pack $ "Minimum allocation must be greater than or equall to 0 but has: " <> show minAlloc
      }
  toApiError (InvalidFee fee) =
    GYApiError
      { gaeErrorCode = "INVALID_FEE"
      , gaeHttpStatus = status400
      , gaeMsg = Txt.pack $ "Fee must be between 0 and 1 but has: " <> show fee
      }

validateSaleRoundInfo :: TokenSaleParams -> IO TokenSaleParams
validateSaleRoundInfo tParam
  | beginSale >= endSale = throwIO . GYApplicationException $ ToEarlyEndSale beginSale endSale
  | endDistribution <= endSale = throwIO . GYApplicationException $ ToEarlyDistribution endSale endDistribution
  | minAllocation < 0 = throwIO . GYApplicationException $ NonPositiveMinAllocation minAllocation
  | fee < 0 || fee > 1 = throwIO . GYApplicationException $ InvalidFee fee
  | otherwise = return tParam
  where
    beginSale = tspBeginSale tParam
    endSale = tspEndSale tParam
    endDistribution = tspEndDistribution tParam
    minAllocation = tspMinAllocation tParam
    fee = tspFee tParam
