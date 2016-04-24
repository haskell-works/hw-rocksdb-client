module RocksDB.ReadOptions

where

import           Control.Monad
import           Control.Monad.IO.Class
import           RocksDB.Internal.C

data ReadOptions = ReadOptions ReadOptionsFPtr
data ReadOptionsBuilder = ReadOptionsBuilder { runReadOptions :: ReadOptions -> IO ReadOptions }

instance Monoid ReadOptionsBuilder where
    mempty = ReadOptionsBuilder return
    mappend a b = ReadOptionsBuilder (runReadOptions a >=> runReadOptions b)

createReadOptions :: ReadOptionsBuilder -> IO ReadOptions
createReadOptions o = (ReadOptions <$> liftIO c_rocksdb_readoptions_create) >>= runReadOptions o

-- | Creates new empty 'ReadOptions'
defaultReadOptions :: IO ReadOptions
defaultReadOptions = createReadOptions mempty

setVerifyChecksums :: Bool -> ReadOptionsBuilder
setVerifyChecksums b = withReadOptions $ flip c_rocksdb_readoptions_set_verify_checksums b

setTailing :: Bool -> ReadOptionsBuilder
setTailing b = withReadOptions $ flip c_rocksdb_readoptions_set_tailing b

setFillCache :: Bool -> ReadOptionsBuilder
setFillCache b = withReadOptions $ flip c_rocksdb_readoptions_set_fill_cache b

-------------------------------------------------------------------------------
withReadOptions :: (ReadOptionsFPtr -> IO ()) -> ReadOptionsBuilder
withReadOptions f = ReadOptionsBuilder $ \(ReadOptions o) -> f o >> return (ReadOptions o)
{-# INLINE withReadOptions #-}
