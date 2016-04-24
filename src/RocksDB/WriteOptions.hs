module RocksDB.WriteOptions
( WriteOptions(..)
, WriteOptionsBuilder
, createWriteOptions
, defaultWriteOptions
, setSync
, disableWAL
)

where

import           Control.Monad
import           Control.Monad.IO.Class
import           RocksDB.Internal.C

data WriteOptions = WriteOptions WriteOptionsFPtr
data WriteOptionsBuilder = WriteOptionsBuilder { runWriteOptions :: WriteOptions -> IO WriteOptions }

instance Monoid WriteOptionsBuilder where
    mempty = WriteOptionsBuilder return
    mappend a b = WriteOptionsBuilder (runWriteOptions a >=> runWriteOptions b)

createWriteOptions :: MonadIO m => WriteOptionsBuilder -> m WriteOptions
createWriteOptions o = liftIO $ (WriteOptions <$> liftIO c_rocksdb_writeoptions_create) >>= runWriteOptions o

-- | Creates new empty 'WriteOptions'
defaultWriteOptions :: MonadIO m => m WriteOptions
defaultWriteOptions = createWriteOptions mempty

setSync :: Bool -> WriteOptionsBuilder
setSync b = withWriteOptions $ flip c_rocksdb_writeoptions_set_sync b

disableWAL :: Bool -> WriteOptionsBuilder
disableWAL b = withWriteOptions $ flip c_rocksdb_writeoptions_disable_WAL b

-------------------------------------------------------------------------------
withWriteOptions :: (WriteOptionsFPtr -> IO ()) -> WriteOptionsBuilder
withWriteOptions f = WriteOptionsBuilder $ \(WriteOptions o) -> f o >> return (WriteOptions o)
{-# INLINE withWriteOptions #-}
