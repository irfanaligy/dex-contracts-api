{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : GeniusYield.Scripts.MerkleRewards.MerkleTree
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.com
Stability   : develop
-}
module GeniusYield.Scripts.MerkleRewards.MerkleTree
  ( MerkleTree
  , depth
  , leaf
  , node
  , zeroHash
  , buildMerkleTree
  , MerkleProofStep (..)
  , MerkleProof
  , merkleProof
  , has
  )
where

import Data.Aeson (KeyValue (..), Value, object, withObject, (.:))
import Data.Aeson.Types (FromJSON (..), Parser, ToJSON (..))
import Data.Maybe (isJust)
import Text.Printf (printf)

import GeniusYield.OnChain.MerkleRewards.MerkleTree (MerkleProof, MerkleProofStep (..))
import GeniusYield.Scripts.MerkleRewards.Hashable

deriving instance Show MerkleProofStep

deriving instance Eq MerkleProofStep

deriving instance Ord MerkleProofStep

data MerkleTree a
  = Leaf Hash (Maybe a)
  | Node Hash (MerkleTree a) (MerkleTree a)
  deriving (Eq, Foldable, Ord, Show)

depth :: MerkleTree a -> Int
depth (Leaf _ _) = 0
depth (Node _ l r)
  | dl == dr = dl + 1
  | otherwise = error "imperfect Merkle Tree"
  where
    dl = depth l
    dr = depth r

instance Hashable (MerkleTree a) where
  hash (Leaf h _) = h
  hash (Node h _ _) = h

instance ToJSON a => ToJSON (MerkleTree a) where
  toJSON :: MerkleTree a -> Value
  toJSON = toJSON . fromMerkleTree

instance (FromJSON a, Hashable a) => FromJSON (MerkleTree a) where
  parseJSON :: Value -> Parser (MerkleTree a)
  parseJSON v = do
    t <- parseJSON v
    pure $ toMerkleTree t

data MerkleTree' a
  = Leaf' (Maybe a)
  | Node' (MerkleTree' a) (MerkleTree' a)
  deriving (Eq, Foldable, Ord, Show)

instance ToJSON a => ToJSON (MerkleTree' a) where
  toJSON :: MerkleTree' a -> Value
  toJSON (Leaf' x) = object ["tag" .= leafTag, "value" .= toJSON x]
  toJSON (Node' l r) = object ["tag" .= nodeTag, "left" .= toJSON l, "right" .= toJSON r]

instance FromJSON a => FromJSON (MerkleTree' a) where
  parseJSON :: Value -> Parser (MerkleTree' a)
  parseJSON = withObject "MerkleTree" $ \o -> do
    tag <- o .: "tag"
    case tag of
      t | t == leafTag -> Leaf' <$> o .: "value"
      t
        | t == nodeTag ->
            Node'
              <$> o .: "left"
              <*> o .: "right"
      t -> fail $ printf "invalid tag %s, expected %s or %s" t leafTag nodeTag

leafTag, nodeTag :: String
leafTag = "Leaf"
nodeTag = "Node"

toMerkleTree :: Hashable a => MerkleTree' a -> MerkleTree a
toMerkleTree (Leaf' x) = leaf x
toMerkleTree (Node' l r) = node (toMerkleTree l) (toMerkleTree r)

fromMerkleTree :: MerkleTree a -> MerkleTree' a
fromMerkleTree (Leaf _ x) = Leaf' x
fromMerkleTree (Node _ l r) = Node' (fromMerkleTree l) (fromMerkleTree r)

leaf :: Hashable a => Maybe a -> MerkleTree a
leaf x = Leaf (hash x) x

node :: MerkleTree a -> MerkleTree a -> MerkleTree a
node l r = Node (hash (l, r)) l r

zeroLeaf :: Hashable a => MerkleTree a
zeroLeaf = leaf Nothing

zeroHash :: Hash
zeroHash = hash $ zeroLeaf @() -- Nothing always has the same hash.

nextPowerOfTwo :: Int -> (Int, Int)
nextPowerOfTwo n = go 0 1
  where
    go :: Int -> Int -> (Int, Int)
    go e p
      | p >= n = (e, p)
      | otherwise = go (e + 1) $ 2 * p

-- | Build a perfect Merkle Tree from a list of leaf values.
buildMerkleTree :: forall a. Hashable a => [a] -> (MerkleTree a, Int)
buildMerkleTree [] = (leaf Nothing, 0)
buildMerkleTree xs = (,e) $ go $ take p $ map (leaf . Just) xs ++ repeat zeroLeaf
  where
    e, p :: Int
    (e, p) = nextPowerOfTwo $ length xs

    go :: [MerkleTree a] -> MerkleTree a
    go [] = error "impossible branch"
    go [x] = x
    go ys = go $ go' ys

    go' :: [MerkleTree a] -> [MerkleTree a]
    go' [] = []
    go' [_] = error "impossible branch"
    go' (x : y : zs) = node x y : go' zs

merkleProof
  :: forall a
   . Hashable a
  => (a -> Bool)
  -- ^ Predicate to find the value to prove.
  -> MerkleTree a
  -- ^ The tree to search.
  -> Maybe (a, MerkleTree a, MerkleProof)
  -- ^ The found value, the tree with this value removed, and the proof.
merkleProof p = fmap (\(v, r, xs) -> (v, r, reverse xs)) . go
  where
    go :: MerkleTree a -> Maybe (a, MerkleTree a, MerkleProof)
    go (Leaf _ Nothing) = Nothing
    go (Leaf _ (Just x))
      | p x = Just (x, zeroLeaf, [])
      | otherwise = Nothing
    go (Node _ l r) = case go l of
      Just (v, l', proof) -> Just (v, node l' r, MPLeft (hash r) : proof)
      Nothing -> case go r of
        Just (v, r', proof) -> Just (v, node l r', MPRight (hash l) : proof)
        Nothing -> Nothing

has :: Hashable a => (a -> Bool) -> MerkleTree a -> Bool
has p = isJust . merkleProof p
