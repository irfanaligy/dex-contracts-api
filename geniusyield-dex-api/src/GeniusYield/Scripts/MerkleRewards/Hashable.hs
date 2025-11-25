{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : GeniusYield.Scripts.MerkleRewards.Hashable
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.com
Stability   : develop
-}
module GeniusYield.Scripts.MerkleRewards.Hashable
  ( Hash
  , bytestringFromHash
  , Hashable (..)
  )
where

import Data.Aeson.Types (FromJSON (..), Parser, ToJSON (..), Value)
import Data.ByteString.Base16 qualified as BS16
import Data.ByteString.Char8 qualified as BS8
import Data.Coerce (coerce)
import Data.Map.Strict qualified as Map
import Data.Text.Encoding qualified as Text
import GeniusYield.Imports (Map, PrintfArg (..), (>>>))
import GeniusYield.Types
  ( GYDatumHash
  , GYPubKeyHash
  , GYValue
  , datumFromPlutusData
  , datumHashToPlutus
  , hashDatum
  , pubKeyHashToPlutus
  , valueToPlutus
  )
import PlutusLedgerApi.V1 qualified as Plutus
import PlutusTx.Builtins (blake2b_256)
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import Text.Printf (FieldFormatter)

import GeniusYield.OnChain.MerkleRewards.MerkleTree (Hash (..), appendHash)

bytestringFromHash :: Hash -> BS8.ByteString
bytestringFromHash = getHash >>> \(BuiltinByteString bs) -> bs

instance Show Hash where
  show :: Hash -> String
  show (Hash (BuiltinByteString bs)) = BS8.unpack $ BS16.encode bs

deriving newtype instance Eq Hash

deriving newtype instance Ord Hash

instance PrintfArg Hash where
  formatArg :: Hash -> FieldFormatter
  formatArg = formatArg . show

instance ToJSON Hash where
  toJSON :: Hash -> Value
  toJSON = toJSON . show

instance FromJSON Hash where
  parseJSON :: Value -> Parser Hash
  parseJSON v = do
    t <- parseJSON v
    case BS16.decode $ Text.encodeUtf8 t of
      Left err -> fail $ "invalid hash " <> show t <> ": " <> show err
      Right bs -> pure $ Hash $ BuiltinByteString bs

class Hashable a where
  hash :: a -> Hash

instance Hashable () where
  hash () = Hash $ blake2b_256 "()"

instance (Hashable a, Hashable b) => Hashable (a, b) where
  hash (x, y) = appendHash (hash x) (hash y)

instance Hashable a => Hashable (Maybe a) where
  hash Nothing = Hash $ blake2b_256 "Nothing"
  hash (Just x) = hash x

instance Hashable a => Hashable [a] where
  hash :: Hashable a => [a] -> Hash
  hash [] = Hash $ blake2b_256 "[]"
  hash (x : xs) = hash (x, xs)

instance (Hashable a, Hashable b) => Hashable (Map a b) where
  hash :: (Hashable a, Hashable b) => Map a b -> Hash
  hash = hash . Map.toList

newtype HashableData a = HashableData {getHashableData :: a}
  deriving (Eq, Ord, Show)

instance Plutus.ToData a => Hashable (HashableData a) where
  hash = coerce . datumHashToPlutus . hashDatum . datumFromPlutusData . getHashableData

instance Hashable Plutus.PubKeyHash where
  hash :: Plutus.PubKeyHash -> Hash
  hash = coerce

instance Hashable Plutus.DatumHash where
  hash :: Plutus.DatumHash -> Hash
  hash = coerce

deriving via (HashableData Plutus.Value) instance Hashable Plutus.Value

instance Hashable GYPubKeyHash where
  hash :: GYPubKeyHash -> Hash
  hash = hash . pubKeyHashToPlutus

instance Hashable GYDatumHash where
  hash :: GYDatumHash -> Hash
  hash = hash . datumHashToPlutus

instance Hashable GYValue where
  hash :: GYValue -> Hash
  hash = hash . valueToPlutus
