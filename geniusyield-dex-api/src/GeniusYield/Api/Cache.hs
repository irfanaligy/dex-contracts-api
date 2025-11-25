module GeniusYield.Api.Cache
  ( Cache (..)
  , cPut'
  , RewardsCache (..)
  , cacheNS
  , withCache
  , mkRewardsCache
  , mkInMemoryCache
  , mkJSONFileCache
  , mkRedisCache
  , withFiFoInMemoryCache
  )
where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar, withMVar)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (decodeFileStrict, decodeStrict, encode, encodeFile)
import Data.ByteString.Lazy (toStrict)
import Data.Foldable (find)
import Data.Map.Strict qualified as Map
import Database.Redis qualified as Redis
import Deriving.Aeson
import GeniusYield.Imports (Exception, throwIO)
import GeniusYield.TxBuilder (GYTxQueryMonad, gyLogInfo', gyLogWarning')
import GeniusYield.Types (GYLogNamespace)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((<.>), (</>))

import GeniusYield.Scripts.MerkleRewards.Hashable (Hashable (..), bytestringFromHash)

-- | Exceptions (or errors) encountered when interacting with cache.
newtype CacheException
  = -- | Failed to insert a key-value pair in Redis.
    RedisPutException (Either Redis.Reply Redis.Status)
  deriving stock Show
  deriving anyclass Exception

data Cache m k v = Cache
  { cGet :: k -> m (Maybe v)
  , cPut :: k -> v -> m (Maybe CacheException)
  }

data RewardsCache = RCInMemory | RCJSONFile !FilePath | RCRedis !String
  deriving stock (Eq, Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[ConstructorTagModifier '[CamelToSnake]] RewardsCache

cacheNS :: GYLogNamespace
cacheNS = "cache"

{-# INLINEABLE cPut' #-}
cPut' :: (GYTxQueryMonad m, MonadIO m, Show k, Show v) => Cache IO k v -> k -> v -> m v
cPut' cache k v = do
  putResult <- liftIO $ cPut cache k v
  case putResult of
    Nothing -> pure v
    Just ce -> do
      gyLogWarning' cacheNS $ "Unable to insert key-value pair, " <> show k <> " - " <> show v <> " into the cache! Error encountered: " <> show ce
      pure v

withCache :: (GYTxQueryMonad m, MonadIO m, Show k, Show v) => Cache IO k v -> (k -> m v) -> (k -> m v)
withCache c@Cache {..} f k = do
  mv <- liftIO $ cGet k
  case mv of
    Just v -> pure v
    Nothing -> do
      gyLogInfo' cacheNS $ "Unable to find key " <> show k <> " in cache."
      v <- f k
      cPut' c k v

mkRewardsCache :: (FromJSON v, Hashable k, MonadIO m, Ord k, ToJSON v) => RewardsCache -> IO (Cache m k v)
mkRewardsCache rewardsCache = case rewardsCache of
  RCInMemory -> mkInMemoryCache
  RCJSONFile fp -> mkJSONFileCache fp
  RCRedis connString -> mkRedisCache connString

mkInMemoryCache :: forall k v m. (MonadIO m, Ord k) => IO (Cache m k v)
mkInMemoryCache = do
  m <- newMVar mempty
  pure
    $ Cache
      { cGet = \k -> liftIO $ Map.lookup k <$> readMVar m
      , cPut = \k v -> liftIO $ fmap (const Nothing) $ modifyMVar_ m $ pure . Map.insert k v
      }

mkJSONFileCache :: forall k v m. (FromJSON v, Hashable k, MonadIO m, ToJSON v) => FilePath -> IO (Cache m k v)
mkJSONFileCache folder = do
  createDirectoryIfMissing True folder
  lock <- newMVar ()
  pure
    $ Cache
      { cGet = liftIO . withMVar lock . const . tryReadFile . keyFile
      , cPut = \k -> liftIO . fmap (const Nothing) . withMVar lock . const . encodeFile (keyFile k)
      }
  where
    keyFile :: k -> FilePath
    keyFile k = folder </> show (hash k) <.> "json"

    tryReadFile :: FilePath -> IO (Maybe v)
    tryReadFile file = do
      exists <- doesFileExist file
      if exists
        then decodeFileStrict file
        else pure Nothing

mkRedisCache :: forall k v m. (FromJSON v, Hashable k, MonadIO m, ToJSON v) => String -> IO (Cache m k v)
mkRedisCache connString = do
  case Redis.parseConnectInfo connString of
    Left e -> throwIO . userError $ "Invalid Redis connection string, " <> connString <> ", error: " <> e
    Right connInfo -> do
      conn <- Redis.checkedConnect connInfo
      pure
        $ Cache
          { cGet = \k -> liftIO $ Redis.runRedis conn $ either (const Nothing) (>>= decodeStrict) <$> Redis.get (bytestringFromHash $ hash k)
          , cPut = \k v -> liftIO $ do
              replyStatus <- Redis.runRedis conn $ Redis.set (bytestringFromHash $ hash k) (toStrict $ encode v)
              case replyStatus of
                Right Redis.Ok -> pure Nothing
                anyOther -> pure $ Just $ RedisPutException anyOther
          }

infixr 5 :>

data StrictList a = Nil | !a :> !(StrictList a)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

strictTake :: Int -> StrictList a -> StrictList a
strictTake n xs
  | n <= 0 = Nil
  | otherwise = case xs of
      Nil -> Nil
      x :> xs' -> x :> strictTake (n - 1) xs'

withFiFoInMemoryCache :: forall k v. Eq k => Int -> Cache IO k v -> IO (Cache IO k v)
withFiFoInMemoryCache capacity Cache {..} = do
  var <- newMVar Nil
  pure
    Cache
      { cGet = gt var
      , cPut = pt var
      }
  where
    gt :: MVar (StrictList (k, v)) -> k -> IO (Maybe v)
    gt var k = modifyMVar var $ \s -> do
      case find ((== k) . fst) s of
        Just (_, v) -> pure (s, Just v) -- if k is already in the in-memory cache, we just return the value
        Nothing -> do
          mv <- cGet k
          case mv of
            Nothing -> pure (s, Nothing) -- if k is not in the underlying cache, we can't do anything
            Just v -> do
              -- otherwise, we can save the value in-memory
              let !s' = putPure k v s
              pure (s', Just v)

    pt :: MVar (StrictList (k, v)) -> k -> v -> IO (Maybe CacheException)
    pt var k v = do
      modifyMVar_ var $! pure . putPure k v -- we put the value into the in-memory cache
      cPut k v -- we also put it into the underlying (persistent) cache
    putPure :: k -> v -> StrictList (k, v) -> StrictList (k, v)
    putPure !k !v s = strictTake capacity $! (k, v) :> s
