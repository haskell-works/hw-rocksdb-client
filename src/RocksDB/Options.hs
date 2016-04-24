{-# LANGUAGE MultiParamTypeClasses #-}
module RocksDB.Options
( Options(..)
, OptionsBuilder
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

data Options = Options OptionsFPtr
data OptionsBuilder = OptionsBuilder { runOptionsBuilder :: Options -> IO Options }

instance Monoid OptionsBuilder where
    mempty = OptionsBuilder return
    mappend a b = OptionsBuilder (runOptionsBuilder a >=> runOptionsBuilder b)

-- | Creates 'Options' given a specification
--
-- @
--    dbOpts = createOptions $ setCompression NoCompression
--                          <> setCompaction LevelCompaction
-- @
createOptions :: OptionsBuilder -> IO Options
createOptions o = (Options <$> c_rocksdb_options_create) >>= runOptionsBuilder o

-- | Creates new 'Options'
defaultOptions :: OptionsBuilder
defaultOptions = mempty

setCompression :: Compression -> OptionsBuilder
setCompression c =
    withOptionsBuilder $ flip c_rocksdb_options_set_compression c

setCompaction :: Compaction -> OptionsBuilder
setCompaction c =
    withOptionsBuilder $ flip c_rocksdb_options_set_compaction_style c

setParallelism :: Int -> OptionsBuilder
setParallelism p =
    withOptionsBuilder $ flip c_rocksdb_options_increase_parallelism p

setCreateIfMissing :: Bool -> OptionsBuilder
setCreateIfMissing b =
    withOptionsBuilder $ flip c_rocksdb_options_set_create_if_missing b

setCreateMissingCF :: Bool -> OptionsBuilder
setCreateMissingCF b =
    withOptionsBuilder $ flip c_rocksdb_options_set_create_missing_column_families b

setErrorIfExists :: Bool -> OptionsBuilder
setErrorIfExists b =
    withOptionsBuilder $ flip c_rocksdb_options_set_error_if_exists b

setParanoidChecks :: Bool -> OptionsBuilder
setParanoidChecks b =
    withOptionsBuilder $ flip c_rocksdb_options_set_paranoid_checks b

setNumLevels :: Int -> OptionsBuilder
setNumLevels n =
    withOptionsBuilder $ flip c_rocksdb_options_set_num_levels n

setUseFsync :: Int -> OptionsBuilder
setUseFsync b =
    withOptionsBuilder $ flip c_rocksdb_options_set_use_fsync b

--------------------------------------------------------------------------------
withOptionsBuilder :: (OptionsFPtr -> IO ()) -> OptionsBuilder
withOptionsBuilder f = OptionsBuilder $ \(Options o) -> f o >> return (Options o)
{-# INLINE withOptionsBuilder #-}

