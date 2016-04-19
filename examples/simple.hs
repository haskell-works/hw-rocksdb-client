{-# LANGUAGE OverloadedStrings #-}
module Simple

where

import           Data.Either.Unwrap
import           Data.Maybe
import           RocksDB.Internal.C as C
import           System.Directory
import           System.FilePath

dbPath :: IO FilePath
dbPath  = (</> "rocksdb_simple_example")       <$> getTemporaryDirectory

bakPath :: IO FilePath
bakPath = (</> "rocksdb_simple_example_bakup") <$> getTemporaryDirectory

main :: IO ()
main = do
    path <- dbPath
    backupPath <- bakPath

    dbo <- c_rocksdb_options_create
    c_rocksdb_options_increase_parallelism dbo 2
    c_rocksdb_options_optimize_level_style_compaction dbo 0
    c_rocksdb_options_set_create_if_missing dbo True

    db     <- c_rocksdb_open dbo path >>= assertNoError
    --backup <- c_rocksdb_backup_engine_open dbo backupPath >>= assertNoError

    -- Put key-value
    writeOpts <- c_rocksdb_writeoptions_create
    _ <- c_rocksdb_put db writeOpts "Key" "Value" >>= assertHasValue

    -- Get key-value
    readOpts <- c_rocksdb_readoptions_create
    resValue <- c_rocksdb_get db readOpts "Key" >>= assertNoError
    print $ "Returned value was: " ++ show resValue


    c_rocksdb_close db

    print "All done"

assertNoError :: Monad m => Either a b -> m b
assertNoError = return . fromRight

assertHasValue :: Monad m => Maybe a -> m a
assertHasValue  = return . fromJust
