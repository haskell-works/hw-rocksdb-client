module RocksDB.Iterator

where

import RocksDB.Types
import Data.ByteString (ByteString)
import RocksDB.Internal.C
import Control.Monad.IO.Class
import RocksDB.ReadOptions

data Iterator = Iterator ReadOptionsFPtr IteratorFPtr
data Iterate a = IterateNext a | IterateCompleted a

createIterator :: MonadIO m => RocksDB -> ReadOptions -> m Iterator
createIterator (RocksDB _ db) (ReadOptions o) =
  liftIO $ Iterator o <$> c_rocksdb_create_iterator db o

createIteratorFrom :: MonadIO m => RocksDB -> ReadOptions -> ByteString -> m Iterator
createIteratorFrom (RocksDB _ db) (ReadOptions o) k = do
  iter <- liftIO $ c_rocksdb_create_iterator db o
  liftIO $ c_rocksdb_iter_seek iter k
  return $ Iterator o iter

foldIterator :: MonadIO m
             => Iterator
             -> (a -> (ByteString, ByteString) -> a)
             -> a
             -> m a
foldIterator i = foldMonad (liftIO $ next i)

foldIterator' :: MonadIO m
              => Iterator
              -> (a -> (ByteString, ByteString) -> Iterate a)
              -> a
              -> m a
foldIterator' i = foldMonad' (liftIO $ next i)

next :: Iterator -> IO (Maybe (ByteString, ByteString))
next (Iterator _ i) = do
  valid <- c_rocksdb_iter_valid i
  if valid
     then do
       c_rocksdb_iter_next i
       key <- c_rocksdb_iter_key i
       val <- c_rocksdb_iter_value i
       return $ (,) <$> key <*> val
     else return Nothing


foldMonad :: Monad m => m (Maybe a) -> (b -> a -> b) -> b -> m b
foldMonad i f z =
  let f' x = IterateNext . f x
  in foldMonad' i f' z

foldMonad' :: Monad m => m (Maybe a) -> (b -> a -> Iterate b) -> b -> m b
foldMonad' i f = go
  where
    go acc = do
      mn <- i
      case mn of
        Nothing -> return acc
        Just res ->
          case f acc res of
            IterateNext a -> go a
            IterateCompleted a -> return a
