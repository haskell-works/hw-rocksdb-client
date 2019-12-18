{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import RocksDB
import RocksDB.Options
import RocksDB.ReadOptions
import RocksDB.WriteOptions
import System.Directory
import System.FilePath

dbPath :: IO FilePath
dbPath  = (</> "rocksdb_simple_example")       <$> getTemporaryDirectory

main :: IO ()
main = runExample >>= (print . show)


runExample :: IO (Either RocksDBError ())
runExample = runExceptT $ do
    path  <- liftIO dbPath
    db    <- open path (setCreateIfMissing True)

    wOpts <- defaultWriteOptions
    rOpts <- defaultReadOptions

    put db wOpts "MyKey" "MyValue"
    res <- get db rOpts "MyKey"
    liftIO $ print (show res)

    mres <- multiGet' db rOpts ["MyKey", "noKey"]
    liftIO $ print (show mres)

    delete db wOpts "MyKey"
    nex <- get db rOpts "MyKey"
    liftIO $ print (show nex)

    close db
