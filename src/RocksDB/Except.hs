module RocksDB.Except
( open, openForReadOnly
, close
, put, get, delete
, multiGet, multiGet'
, module RocksDB.Types
)
where

import           Control.Error.Util
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.ByteString            (ByteString)
import           RocksDB.Options
import           RocksDB.ReadOptions
import           RocksDB.Types
import           RocksDB.WriteOptions
import qualified RocksDB.Basic as B

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
     -> ExceptT RocksDBError m RocksDB -- ^ RocksDB handle. Mush be closed with the 'close' function.
open p o = ExceptT $ B.open p o

-- | Opens RocksDB database in read only mode
openForReadOnly :: MonadIO m
                => FilePath              -- ^ Path where the database files are located
                -> OptionsBuilder        -- ^ Database options (see 'OptionsBuilder').
                -> ErrorIfExists         -- ^ Error if log file exists
                -> ExceptT RocksDBError m RocksDB -- ^ RocksDB handle. Mush be closed with the 'close' function.
openForReadOnly p o e = ExceptT $ B.openForReadOnly p o e

-- | Closes the database handle.
close :: MonadIO m => RocksDB -> ExceptT RocksDBError m ()
close r =
  B.close r >>= (hoistEither . Right)

-- | Puts a given key/value pair to the database.
put :: MonadIO m
    => RocksDB           -- ^ RocksDB handle
    -> WriteOptions      -- ^ Write operation options
    -> ByteString        -- ^ Key
    -> ByteString        -- ^ Value
    -> ExceptT RocksDBError m ()  -- ^ Result may contain error, '()' if successful
put r o k v =
  maybeErrorResult $ B.put r o k v

-- | Gets a value for a given key
get :: MonadIO m
    => RocksDB                          -- ^ RocksDB handle
    -> ReadOptions                      -- ^ Read operation options
    -> ByteString                       -- ^ Key
    -> ExceptT RocksDBError m (Maybe ByteString) -- ^ Possibly value if exists for a given key
get r o k = ExceptT $ B.get r o k

-- | Gets values for multiple keys
multiGet :: MonadIO m
         => RocksDB                          -- ^ RocksDB handle
         -> ReadOptions                      -- ^ Read operation options
         -> [ByteString]                     -- ^ A list of keys
         -> ExceptT RocksDBError m [Maybe ByteString] -- ^ A list of possible values for each key, in the same order with the keys
multiGet r o ks = ExceptT $ B.multiGet r o ks

-- | Gets values for multiple keys
multiGet' :: MonadIO m
          => RocksDB                                  -- ^ RocksDB handle
          -> ReadOptions                              -- ^ Read operation options
          -> [ByteString]                             -- ^ A list of keys
          -> ExceptT RocksDBError m [(ByteString, ByteString)] -- ^ List of key/value pairs, only contains key/values for keys that have values
multiGet' r o ks = ExceptT $ B.multiGet' r o ks

-- | Deletes a given key from the database
delete :: MonadIO m
       => RocksDB          -- ^ RocksDB handle
       -> WriteOptions     -- ^ Write operation options
       -> ByteString       -- ^ A key to delete
       -> ExceptT RocksDBError m () -- ^ Result may contain error, '()' if successful
delete r o k = maybeErrorResult $ B.delete r o k
-------------------------------------------------------------------------------
maybeErrorResult :: MonadIO m => m (Maybe a) -> ExceptT a m ()
maybeErrorResult ma = ExceptT $ maybe (Right ()) Left <$> ma
{-# INLINE maybeErrorResult #-}
