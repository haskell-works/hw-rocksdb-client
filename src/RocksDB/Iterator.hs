module RocksDB.Iterator
( Iterator
, Iterate (..)
, runIterator, runIterator'
, runIteratorFrom, runIteratorFrom'
)
where

import Control.Exception
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import RocksDB.Internal.C
import RocksDB.ReadOptions
import RocksDB.Types

data Iterator = Iterator ReadOptionsFPtr IteratorFPtr
data Iterate a = IterateNext a | IterateCompleted a

runIterator :: MonadIO m
            => RocksDB
            -> ReadOptions
            -> a
            -> (a -> (ByteString, ByteString) -> a)
            -> m a
runIterator db o z f=
  liftIO $ bracket (createIterator db o)
                    closeIterator
                    (\i -> foldMonad (next i) f z)

runIterator' :: MonadIO m
             => RocksDB
             -> ReadOptions
             -> a
             -> (a -> (ByteString, ByteString) -> Iterate a)
             -> m a
runIterator' db o z f=
  liftIO $ bracket (createIterator db o)
                    closeIterator
                    (\i -> foldMonad' (next i) f z)

runIteratorFrom :: MonadIO m
                => RocksDB
                -> ReadOptions
                -> a
                -> ByteString
                -> (a -> (ByteString, ByteString) -> a)
                -> m a
runIteratorFrom db o z k f =
  liftIO $ bracket (createIteratorFrom db o k)
                   closeIterator
                   (\i -> foldMonad (next i) f z)

runIteratorFrom' :: MonadIO m
                 => RocksDB
                 -> ReadOptions
                 -> a
                 -> ByteString
                 -> (a -> (ByteString, ByteString) -> Iterate a)
                 -> m a
runIteratorFrom' db o z k f =
  liftIO $ bracket (createIteratorFrom db o k)
                   closeIterator
                   (\i -> foldMonad' (next i) f z)

createIterator :: RocksDB -> ReadOptions -> IO Iterator
createIterator (RocksDB _ db) (ReadOptions o) = do
  iter <- c_rocksdb_create_iterator db o
  c_rocksdb_iter_seek_to_first iter
  return $ Iterator o iter

createIteratorFrom :: RocksDB -> ReadOptions -> ByteString -> IO Iterator
createIteratorFrom (RocksDB _ db) (ReadOptions o) k = do
  iter <- c_rocksdb_create_iterator db o
  c_rocksdb_iter_seek iter k
  return $ Iterator o iter

closeIterator :: Iterator -> IO ()
closeIterator (Iterator _ i) = c_rocksdb_iter_destroy i

next :: Iterator -> IO (Maybe (ByteString, ByteString))
next (Iterator _ i) = do
  valid <- c_rocksdb_iter_valid i
  if valid
    then do
      key <- c_rocksdb_iter_key i
      val <- c_rocksdb_iter_value i
      c_rocksdb_iter_next i
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
