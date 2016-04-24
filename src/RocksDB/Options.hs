{-# LANGUAGE MultiParamTypeClasses #-}
module RocksDB.Options
( Options
, defaultOptions
, createOptions
, setCompaction
, setCompression
, setParallelism
, setCreateIfMissing
, setCreateMissingCF
, setErrorIfExists
, setParanoidChecks
, setNumLevels
, setUseFsync
)

where

import           Control.Monad
import           RocksDB.Internal.C

data Options = Options { runOptions :: OptionsFPtr -> IO OptionsFPtr }

instance Monoid Options where
    mempty = Options return
    mappend a b = Options (runOptions a >=> runOptions b)

-- | Creates 'Options' given a specification
--
-- @
--    dbOpts = createOptions $ setCompression NoCompression
--                          <> setCompaction LevelCompaction
-- @
createOptions :: Options -> IO OptionsFPtr
createOptions o = c_rocksdb_options_create >>= runOptions o

-- | Creates new 'Options'
defaultOptions :: IO OptionsFPtr
defaultOptions = createOptions mempty

setCompression :: Compression -> Options
setCompression c =
    withOptions $ flip c_rocksdb_options_set_compression c

setCompaction :: Compaction -> Options
setCompaction c =
    withOptions $ flip c_rocksdb_options_set_compaction_style c

setParallelism :: Int -> Options
setParallelism p =
    withOptions $ flip c_rocksdb_options_increase_parallelism p

setCreateIfMissing :: Bool -> Options
setCreateIfMissing b =
    withOptions $ flip c_rocksdb_options_set_create_if_missing b

setCreateMissingCF :: Bool -> Options
setCreateMissingCF b =
    withOptions $ flip c_rocksdb_options_set_create_missing_column_families b

setErrorIfExists :: Bool -> Options
setErrorIfExists b =
    withOptions $ flip c_rocksdb_options_set_error_if_exists b

setParanoidChecks :: Bool -> Options
setParanoidChecks b =
    withOptions $ flip c_rocksdb_options_set_paranoid_checks b

setNumLevels :: Int -> Options
setNumLevels n =
    withOptions $ flip c_rocksdb_options_set_num_levels n

setUseFsync :: Int -> Options
setUseFsync b =
    withOptions $ flip c_rocksdb_options_set_use_fsync b

--------------------------------------------------------------------------------
withOptions :: (OptionsFPtr -> IO ()) -> Options
withOptions f = Options $ \o -> f o >> return o
{-# INLINE withOptions #-}

