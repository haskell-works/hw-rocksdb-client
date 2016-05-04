module RocksDB.Iterator
( Iterator
, Iterate (..)
, runIterator, runIterator'
, runIteratorFrom, runIteratorFrom'
, runIteratorBackwards, runIteratorBackwards'
, runIteratorBackwardsFrom, runIteratorBackwardsFrom'
)
where

import Control.Exception
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import RocksDB.Internal.C
import RocksDB.ReadOptions
import RocksDB.Types

-- | RocksDB Iterator. There is no need to use it directly.
data Iterator = Iterator ReadOptionsFPtr IteratorFPtr

-- | Iterator step indicating how to proceed with the next iteration.
data Iterate a = IterateNext a       -- ^ Iteration result, next iteration needed
               | IterateCompleted a  -- ^ Iteration result, stop iterating

data IteratorSeek = SeekFirst | SeekLast | SeekAt ByteString
data IteratorMove = MovePrev | MoveNext

-- | Runs iterator through the whole database (from a smallest key to a largest one)
runIterator :: MonadIO m
            => RocksDB                              -- ^ RocksDB handle
            -> ReadOptions                          -- ^ Read options
            -> a                                    -- ^ Initial value
            -> (a -> (ByteString, ByteString) -> a) -- ^ Folding function
            -> m a                                  -- ^ Returns a result of a folding function
runIterator = performIterator MoveNext SeekFirst

-- | Runs iterator through the whole database backwards (from a largest ket to a smallest one)
runIteratorBackwards :: MonadIO m
                     => RocksDB                              -- ^ RocksDB handle
                     -> ReadOptions                          -- ^ Read options
                     -> a                                    -- ^ Initial value
                     -> (a -> (ByteString, ByteString) -> a) -- ^ Folding function
                     -> m a                                  -- ^ Returns a result of a folding function
runIteratorBackwards = performIterator MovePrev SeekLast

-- | Runs iterator from the smallest key forward to the largest key giving the ability to stop at any point
runIterator' :: MonadIO m
             => RocksDB                                       -- ^ RocksDB handle
             -> ReadOptions                                   -- ^ Read options
             -> a                                             -- ^ Initial value
             -> (a -> (ByteString, ByteString) -> Iterate a)  -- ^ Folding function. Returning 'IterateCompleted' will stop the iterator.
             -> m a                                           -- ^ Returns a result of a folding function
runIterator' = performIterator' MoveNext SeekFirst

-- | Runs iterator from the largest key to the smallest key giving the ability to stop at any point
runIteratorBackwards' :: MonadIO m
                      => RocksDB                                       -- ^ RocksDB handle
                      -> ReadOptions                                   -- ^ Read options
                      -> a                                             -- ^ Initial value
                      -> (a -> (ByteString, ByteString) -> Iterate a)  -- ^ Folding function. Returning 'IterateCompleted' will stop the iterator.
                      -> m a                                           -- ^ Returns a result of a folding function
runIteratorBackwards' = performIterator' MovePrev SeekLast

-- | Runs iterator from the specified key forward (from smallest to largest)
runIteratorFrom :: MonadIO m
                => RocksDB                               -- ^ RocksDB handle
                -> ReadOptions                           -- ^ Read options
                -> a                                     -- ^ Initial value
                -> ByteString                            -- ^ Starting key
                -> (a -> (ByteString, ByteString) -> a)  -- ^ Folding function
                -> m a                                   -- ^ Returns a result of a folding function
runIteratorFrom db o z k =
  performIterator MoveNext (SeekAt k) db o z

-- | Runs iterator from the specified key backwards (from largest to smallest)
runIteratorBackwardsFrom :: MonadIO m
                         => RocksDB                              -- ^ RocksDB handle
                         -> ReadOptions                          -- ^ Read options
                         -> a                                    -- ^ Initial value
                         -> ByteString                           -- ^ Starting key
                         -> (a -> (ByteString, ByteString) -> a) -- ^ Folding function
                         -> m a                                  -- ^ Returns a result of a folding function
runIteratorBackwardsFrom db o z k =
  performIterator MovePrev (SeekAt k) db o z

-- | Runs iterator from the specified key forward (from smallest to largest) 
-- giving the ability to stop at any point
runIteratorFrom' :: MonadIO m
                 => RocksDB                                      -- ^ RocksDB handle
                 -> ReadOptions                                  -- ^ Read options
                 -> a                                            -- ^ Initial value
                 -> ByteString                                   -- ^ Starting key
                 -> (a -> (ByteString, ByteString) -> Iterate a) -- ^ Folding function. Returning 'IterateCompleted' will stop the iterator.
                 -> m a                                          -- ^ Returns a result of a folding function
runIteratorFrom' db o z k =
  performIterator' MoveNext (SeekAt k) db o z

-- | Runs iterator from the specified key backwards (from largest to smallest)
-- giving the ability to stop at any point
runIteratorBackwardsFrom' :: MonadIO m
                          => RocksDB                                      -- ^ RocksDB handle
                          -> ReadOptions                                  -- ^ Read options
                          -> a                                            -- ^ Initial value
                          -> ByteString                                   -- ^ Starting key
                          -> (a -> (ByteString, ByteString) -> Iterate a) -- ^ Folding function. Returning 'IterateCompleted' will stop the iterator.
                          -> m a                                          -- ^ Returns a result of a folding function
runIteratorBackwardsFrom' db o z k =
  performIterator' MovePrev (SeekAt k) db o z

performIterator :: MonadIO m
                => IteratorMove
                -> IteratorSeek
                -> RocksDB
                -> ReadOptions
                -> a
                -> (a -> (ByteString, ByteString) -> a)
                -> m a
performIterator s p db o z f =
  liftIO $ bracket (createIterator db o p)
                   closeIterator
                   (\i -> foldMonad (move s i) f z)

performIterator' :: MonadIO m
                 => IteratorMove
                 -> IteratorSeek
                 -> RocksDB
                 -> ReadOptions
                 -> a
                 -> (a -> (ByteString, ByteString) -> Iterate a)
                 -> m a
performIterator' s p db o z f =
  liftIO $ bracket (createIterator db o p)
                   closeIterator
                   (\i -> foldMonad' (move s i) f z)

createIterator :: RocksDB -> ReadOptions -> IteratorSeek -> IO Iterator
createIterator (RocksDB _ db) (ReadOptions o) pos = do
  iter <- c_rocksdb_create_iterator db o
  case pos of
    SeekFirst -> c_rocksdb_iter_seek_to_first iter
    SeekLast  -> c_rocksdb_iter_seek_to_last iter
    SeekAt k  -> c_rocksdb_iter_seek iter k
  return $ Iterator o iter

closeIterator :: Iterator -> IO ()
closeIterator (Iterator _ i) = c_rocksdb_iter_destroy i

move :: IteratorMove -> Iterator -> IO (Maybe (ByteString, ByteString))
move step (Iterator _ i) = do
  valid <- c_rocksdb_iter_valid i
  if valid
    then do
      key <- c_rocksdb_iter_key i
      val <- c_rocksdb_iter_value i
      case step of
        MoveNext -> c_rocksdb_iter_next i
        MovePrev -> c_rocksdb_iter_prev i
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
