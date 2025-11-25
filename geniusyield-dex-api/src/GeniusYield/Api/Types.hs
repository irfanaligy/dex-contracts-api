{-# LANGUAGE ConstraintKinds #-}

module GeniusYield.Api.Types
  ( GYApiQueryMonad
  , GYApiMonad
  , Secret (..)
  )
where

import Control.Monad.Reader (MonadReader)
import GeniusYield.TxBuilder.Class (GYTxQueryMonad, GYTxUserQueryMonad)

import GeniusYield.Scripts (GYCompiledScripts)

type GYApiQueryMonad m = (MonadReader GYCompiledScripts m, GYTxQueryMonad m)

type GYApiMonad m = (GYApiQueryMonad m, GYTxUserQueryMonad m)

newtype Secret a = Secret {getSecret :: a}
  deriving (Eq, Ord)

instance Show a => Show (Secret a) where
  show (Secret x) = replicate (length $ show x) '*'
