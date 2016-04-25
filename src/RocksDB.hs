module RocksDB

where

import           Control.Error.Util
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.ByteString            (ByteString)
import           RocksDB.Internal.C
import           RocksDB.Options
import           RocksDB.ReadOptions
import           RocksDB.Types
import           RocksDB.WriteOptions
import           System.FilePath

data RocksDB = RocksDB OptionsFPtr RocksDBFPtr

type RocksDBResult a = ExceptT RocksDBError IO a
newtype ErrorIfExists = ErrorIfExists Bool

open :: FilePath -> OptionsBuilder -> RocksDBResult RocksDB
open p o = do
    (Options opt) <- liftIO $ createOptions o
    res <- liftIO $ c_rocksdb_open opt p
    hoistEither $ RocksDB opt <$> res

openForReadOnly :: FilePath -> OptionsBuilder -> ErrorIfExists -> RocksDBResult RocksDB
openForReadOnly p o (ErrorIfExists e) = do
    (Options opt) <- liftIO $ createOptions o
    res <- liftIO $ c_rocksdb_open_for_read_only opt p e
    hoistEither $ RocksDB opt <$> res

close :: RocksDB -> RocksDBResult ()
close (RocksDB _ r) =
    liftIO (c_rocksdb_close r) >>= (hoistEither . Right)

put :: RocksDB -> WriteOptions -> ByteString -> ByteString -> RocksDBResult ()
put (RocksDB _ r) (WriteOptions o) k v =
    maybeErrorResult $ c_rocksdb_put r o k v

get :: RocksDB -> ReadOptions -> ByteString -> RocksDBResult ByteString
get (RocksDB _ r) (ReadOptions o) k =
    liftIO (c_rocksdb_get r o k) >>= hoistEither

multiGet :: RocksDB -> ReadOptions -> [ByteString] -> RocksDBResult [Maybe ByteString]
multiGet (RocksDB _ r) (ReadOptions o) ks =
    liftIO (c_rocksdb_multi_get r o ks) >>= hoistEither

delete :: RocksDB -> WriteOptions -> ByteString -> RocksDBResult ()
delete (RocksDB _ r) (WriteOptions o) k =
    maybeErrorResult $ c_rocksdb_delete r o k
-------------------------------------------------------------------------------
maybeErrorResult :: MonadIO m => IO (Maybe a) -> ExceptT a m ()
maybeErrorResult ma =
  liftIO ma >>= (hoistEither . maybeErrorToEither)
  where
      maybeErrorToEither x = case x of
          Just x' -> Left x'
          Nothing -> Right ()

