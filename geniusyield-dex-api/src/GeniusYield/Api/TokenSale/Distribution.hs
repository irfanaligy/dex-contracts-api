module GeniusYield.Api.TokenSale.Distribution
  ( calculateDistribution
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GeniusYield.Imports
import GeniusYield.Types

import GeniusYield.Api.TokenSale.Types

calculateDistribution
  :: TokenSaleParams
  -- ^ Token sale parameters.
  -> Natural
  -- ^ Total token supply.
  -> Natural
  -- ^ Maximal token allocation
  -> Set GYAddress
  -- ^ Whitelist of elligible addresses.
  -> Map GYTxOutRef OrderInfo
  -- ^ Orders.
  -> Map GYTxOutRef Natural
  -- ^ Distributed tokens.
calculateDistribution TokenSaleParams {..} supply maxAllocation whiteList allocs = calcDistributionForStakeKey tspMinAllocation (groupByStakeKeyHash whitelistedOrders) $ Map.fromList $ Map.elems $ second calcTokens <$> consolidated
  where
    -- valid offers, given by their order reference and the tokens requested on top of the minimal allocation
    consolidated :: Map GYStakeKeyHashString (GYStakeKeyHashString, Natural)
    consolidated =
      let groupedStakeKeyHash = groupByStakeKeyHash whitelistedOrders
      in Map.foldlWithKey' f Map.empty groupedStakeKeyHash
      where
        f
          :: Map GYStakeKeyHashString (GYStakeKeyHashString, Natural)
          -> GYStakeKeyHashString
          -> [OrderInfo]
          -> Map GYStakeKeyHashString (GYStakeKeyHashString, Natural)
        f m sHash (calcRequest -> totReq)
          | totReq < tspMinAllocation = m
          | otherwise =
              Map.insertWith g sHash (sHash, totReq - tspMinAllocation) m

        g :: (GYStakeKeyHashString, Natural) -> (GYStakeKeyHashString, Natural) -> (GYStakeKeyHashString, Natural)
        g new@(_, newRequest) old@(_, oldRequest)
          | newRequest > oldRequest = new
          | otherwise = old

    -- number of valid offers
    count :: Natural
    count = fromIntegral $ Map.size consolidated

    -- total tokens requested on top of the minimal allocation
    excess :: Natural
    excess = sum $ snd <$> consolidated

    -- number of tokens available for distribution after every valid offer receives the minimal amount
    available :: Natural
    available = supply - count * tspMinAllocation

    -- ratio of excess that every offer can receive
    ratio :: Rational
    ratio = fromIntegral (min available excess) / fromIntegral excess

    -- given the number of tokens requested in excess of the minimal allocation, calculates the total number of tokens the bidder receives
    calcTokens :: Natural -> Natural
    calcTokens e = tspMinAllocation + floor (fromIntegral e * ratio)

    -- given list of orders for address, returns maximum number of token can be requested
    calcRequest :: [OrderInfo] -> Natural
    calcRequest oInfos =
      let
        orderWithMinAlloc = filter (\o -> oiRequest o >= tspMinAllocation) oInfos
        totReq = foldr (\o a -> a + oiRequest o) 0 orderWithMinAlloc
      in
        if totReq > maxAllocation then maxAllocation else totReq

    -- Filters Orders by whitelisted address
    whitelistedOrders :: Map GYTxOutRef OrderInfo
    whitelistedOrders = Map.filter (\OrderInfo {..} -> oiOwnerAddr `Set.member` whiteList) allocs

    groupByStakeKeyHash :: Map GYTxOutRef OrderInfo -> Map GYStakeKeyHashString [OrderInfo]
    groupByStakeKeyHash mInfo = Map.delete "" $ Map.foldl g Map.empty mInfo
      where
        g :: Map GYStakeKeyHashString [OrderInfo] -> OrderInfo -> Map GYStakeKeyHashString [OrderInfo]
        g mInfos o@OrderInfo {..} =
          case stakeKeyFromAddress oiOwnerAddr of
            Nothing -> mInfos
            Just key -> Map.alter (\v -> Just $ o : (fromMaybe [] v)) key mInfos

calcDistributionForStakeKey
  :: Natural
  -- ^ The Minimum Allocation
  -> Map GYStakeKeyHashString [OrderInfo]
  -- ^ The map of StakingHash and OrderInfo
  -> Map GYStakeKeyHashString Natural
  -- ^ The map of stakingHash and allocated amount
  -> Map GYTxOutRef Natural
  -- ^ The map of utxoRef and amount to distribute
calcDistributionForStakeKey minAllocation mInfos msAlloc = Map.unions $ Map.elems $ Map.mapWithKey (pickOrdersForStakeKey) msAlloc
  where
    sortedOrders :: [OrderInfo] -> [OrderInfo]
    sortedOrders = sortBy (\o1 o2 -> compare (oiRequest o2) (oiRequest o1))

    pickOrdersForStakeKey :: GYStakeKeyHashString -> Natural -> Map GYTxOutRef Natural
    pickOrdersForStakeKey k n =
      case Map.lookup k mInfos of
        Nothing -> Map.empty
        Just o -> Map.fromList $ handleOrderForKey (sortedOrders o) n

    -- Fetch Ref with amount to fill for each group of orders
    -- We pick orders such that it can fill token Calculated for StakeKey
    handleOrderForKey :: [OrderInfo] -> Natural -> [(GYTxOutRef, Natural)]
    handleOrderForKey [] _ = []
    handleOrderForKey (OrderInfo {..} : os) num
      -- order left to distribute is less than minAllocation
      | num < minAllocation = []
      -- order can be filled fully
      | oiRequest <= num = (oiRef, oiRequest) : handleOrderForKey os (num - oiRequest)
      -- requested amount is more than amount left to distribute
      -- only fill partially
      | otherwise = [(oiRef, num)]
