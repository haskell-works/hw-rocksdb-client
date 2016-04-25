{-# LANGUAGE OverloadedStrings #-}
module RocksDBSimple

where
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           RocksDB
import           RocksDB.Options
import           RocksDB.ReadOptions
import           RocksDB.Types
import           RocksDB.WriteOptions
import           System.Directory
import           System.FilePath

dbPath :: IO FilePath
dbPath  = (</> "rocksdb_simple_example")       <$> getTemporaryDirectory

main :: IO ()
main = runExample >>= (print . show)


runExample = runExceptT $ do
    path  <- liftIO dbPath
    db    <- open path (setCreateIfMissing True)

    wOpts <- defaultWriteOptions
    rOpts <- defaultReadOptions

    put db wOpts "MyKey" "MyValue"
    res <- get db rOpts "MyKey"
    liftIO $ print (show res)

    nex <- get db rOpts "NexKey"
    liftIO $ print $ "Nex: " ++ show nex


    mres <- multiGet' db rOpts ["MyKey", "noKey"]
    liftIO $ print (show mres)

    close db
