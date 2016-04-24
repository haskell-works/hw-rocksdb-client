{-# LANGUAGE OverloadedStrings #-}
module LowLevelSimple

where

import           Control.Exception
import           Data.Maybe
import           RocksDB.Internal.C as C
import           RocksDB.Types
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

    db <- c_rocksdb_open dbo path >>= assertHasValue

    -- Put key-value
    writeOpts <- c_rocksdb_writeoptions_create
    _ <- c_rocksdb_put db writeOpts "Key" "Value" >>= assertNoError

    -- Get key-value
    readOpts <- c_rocksdb_readoptions_create
    resValue <- c_rocksdb_get db readOpts "Key" >>= assertHasValue
    print $ "Returned value was: " ++ show resValue

    -- Backup
    be <- c_rocksdb_backup_engine_open dbo backupPath >>= assertHasValue
    _ <- c_rocksdb_backup_engine_create_new_backup be db >>= assertNoError

    c_rocksdb_close db

    -- Restore
    restoreOptions <- c_rocksdb_restore_options_create
    _ <- c_rocksdb_backup_engine_restore_db_from_latest_backup be path path restoreOptions >>= assertNoError

    db2 <- c_rocksdb_open dbo path >>= assertHasValue
    resValue <- c_rocksdb_get db2 readOpts "Key" >>= assertHasValue
    print $ "Returned value from backup was: " ++ show resValue

    c_rocksdb_close db2

    print "All done"

assertHasValue :: Either RocksDBError b -> IO b
assertHasValue = return . either (\(RocksDBError e) -> error e) id

assertNoError :: Maybe RocksDBError -> IO ()
assertNoError = return . maybe () (\(RocksDBError e) -> error e)


