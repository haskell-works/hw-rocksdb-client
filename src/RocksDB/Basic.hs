{-# LANGUAGE TupleSections #-}
module RocksDB.Basic
( open, openForReadOnly
, close
, put, get, delete
, multiGet, multiGet'
, module RocksDB.Types
)
where

import Control.Monad.IO.Class
import Data.ByteString        (ByteString)
import Data.Maybe
import RocksDB.Internal.C
import RocksDB.Options
import RocksDB.ReadOptions
import RocksDB.Types
import RocksDB.WriteOptions

-- | Opens a RocksDB database with a given path and options
-- Note that the database must be closed with the 'close' function to avoid leaks.
-- @
--    runExceptT $ do
--        db <- open path (setCreateIfMissing True)
--        ...
-- @
open :: MonadIO m
     => FilePath              -- ^ Path where the database files are located
     -> OptionsBuilder        -- ^ Database options (see 'OptionsBuilder').
     -> m (Either RocksDBError RocksDB) -- ^ RocksDB handle. Mush be closed with the 'close' function.
open p o = liftIO $ do
  (Options opt) <- createOptions o
  res <- c_rocksdb_open opt p
  case res of
    Left (RocksDBError err) -> return . Left . RocksDBError $ "Open error: " ++ err
    Right db                -> return . Right $ RocksDB opt db
  --return $ RocksDB opt <$> res

-- | Opens RocksDB database in read only mode
openForReadOnly :: MonadIO m
                => FilePath              -- ^ Path where the database files are located
                -> OptionsBuilder        -- ^ Database options (see 'OptionsBuilder').
                -> ErrorIfExists         -- ^ Error if log file exists
                -> m (Either RocksDBError RocksDB) -- ^ RocksDB handle. Mush be closed with the 'close' function.
openForReadOnly p o (ErrorIfExists e) = liftIO $ do
  (Options opt) <- createOptions o
  res <- c_rocksdb_open_for_read_only opt p e
  return $ RocksDB opt <$> res

-- | Closes the database handle.
close :: MonadIO m => RocksDB -> m ()
close (RocksDB _ r) = liftIO $ c_rocksdb_close r

-- | Puts a given key/value pair to the database.
put :: MonadIO m
    => RocksDB           -- ^ RocksDB handle
    -> WriteOptions      -- ^ Write operation options
    -> ByteString        -- ^ Key
    -> ByteString        -- ^ Value
    -> m (Maybe RocksDBError) -- ^ Result may contain error, '()' if successful
put (RocksDB _ r) (WriteOptions o) k v =
  liftIO $ c_rocksdb_put r o k v

-- | Gets a value for a given key
get :: MonadIO m
    => RocksDB                          -- ^ RocksDB handle
    -> ReadOptions                      -- ^ Read operation options
    -> ByteString                       -- ^ Key
    -> m (Either RocksDBError (Maybe ByteString)) -- ^ Possibly value if exists for a given key
get (RocksDB _ r) (ReadOptions o) k =
  liftIO $ c_rocksdb_get r o k

-- | Gets values for multiple keys
multiGet :: MonadIO m
         => RocksDB                          -- ^ RocksDB handle
         -> ReadOptions                      -- ^ Read operation options
         -> [ByteString]                     -- ^ A list of keys
         -> m (Either RocksDBError [Maybe ByteString]) -- ^ A list of possible values for each key, in the same order with the keys
multiGet (RocksDB _ r) (ReadOptions o) ks =
  liftIO $ c_rocksdb_multi_get r o ks

-- | Gets values for multiple keys
multiGet' :: MonadIO m
          => RocksDB                                  -- ^ RocksDB handle
          -> ReadOptions                              -- ^ Read operation options
          -> [ByteString]                             -- ^ A list of keys
          -> m (Either RocksDBError [(ByteString, ByteString)]) -- ^ List of key/value pairs, only contains key/values for keys that have values
multiGet' r o ks = do
  res <- multiGet r o ks
  return $ toPairs <$> res
  where
    toPairs vs = catMaybes $ zipWith (\k v -> (k,) <$> v) ks vs

-- | Deletes a given key from the database
delete :: MonadIO m
       => RocksDB          -- ^ RocksDB handle
       -> WriteOptions     -- ^ Write operation options
       -> ByteString       -- ^ A key to delete
       -> m (Maybe RocksDBError) -- ^ Result may contain error, '()' if successful
delete (RocksDB _ r) (WriteOptions o) k =
  liftIO $ c_rocksdb_delete r o k
